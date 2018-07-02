{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module Cardano.Wallet.Server.Plugins (
      Plugin
    , syncWalletWorker
    , acidCleanupWorker
    , conversation
    , legacyWalletBackend
    , walletBackend
    , walletDocumentation
    , resubmitterPlugin
    , notifierPlugin
    ) where

import           Universum

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import           Cardano.Wallet.API.V1.Headers (applicationJson)
import           Cardano.Wallet.Kernel (PassiveWallet)
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), isDebugMode,
                     walletAcidInterval, walletDbOptions)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import           Control.Exception (fromException)
import           Data.Aeson
import           Data.Aeson.Types (Value (..), typeMismatch)
import           Formatting (build, sformat, (%))
import           Generics.SOP.TH (deriveGeneric)
import           GHC.Generics (Generic)
import           Network.HTTP.Types (httpVersionNotSupported505)
import           Network.HTTP.Types.Status (badRequest400)
import           Network.HTTP.Types.Version (http20)
import           Network.Wai (Application, Middleware, Response, httpVersion,
                     ifRequest, modifyResponse, responseHeaders, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings,
                     setOnExceptionResponse)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)
import           Ntp.Client (NtpStatus)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Configuration (walletProductionApi,
                     walletTxCreationDisabled)
import           Pos.Context (HasNodeContext)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (lensOf)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web (cleanupAcidStatePeriodically)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Pending.Worker (startPendingTxsResubmitter)
import           Pos.Wallet.Web.Server.Launcher (walletDocumentationImpl,
                     walletServeImpl)
import           Pos.Wallet.Web.Sockets (getWalletWebSockets,
                     upgradeApplicationWS)
import           Pos.Wallet.Web.State (askWalletDB)
import           Pos.Wallet.Web.Tracking.Sync (processSyncRequest)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (serveWeb)
import           Pos.WorkMode (WorkMode)
import           System.Wlog (logInfo, modifyLoggerName, usingLoggerName)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.LegacyServer as LegacyServer
import qualified Cardano.Wallet.Server as Server
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel
import qualified Data.HashMap.Strict as HMS
import qualified Pos.Wallet.Web.Error.Types as V0
import qualified Pos.Wallet.Web.Server.Runner as V0
import qualified Servant


-- A @Plugin@ running in the monad @m@.
type Plugin m = [Diffusion m -> m ()]

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidCleanupWorker :: WalletBackendParams
                  -> Plugin WalletWebMode
acidCleanupWorker WalletBackendParams{..} = pure $ const $
    modifyLoggerName (const "acidcleanup") $
    askWalletDB >>= \db -> cleanupAcidStatePeriodically db (walletAcidInterval walletDbOptions)

-- | The @Plugin@ which defines part of the conversation protocol for this node.
conversation :: HasConfigurations => WalletBackendParams -> Plugin WalletWebMode
conversation wArgs = map const (pluginsMonitoringApi wArgs)
  where
    pluginsMonitoringApi :: (WorkMode ctx m , HasNodeContext ctx)
                         => WalletBackendParams
                         -> [m ()]
    pluginsMonitoringApi WalletBackendParams {..}
        | enableMonitoringApi = [serveWeb monitoringApiPort walletTLSParams]
        | otherwise = []

walletDocumentation
    :: (HasConfigurations, HasCompileInfo)
    => WalletBackendParams
    -> Plugin WalletWebMode
walletDocumentation WalletBackendParams {..} = pure $ \_ ->
    walletDocumentationImpl
        application
        walletDocAddress
        tls
        (Just defaultSettings)
        Nothing
  where
    application :: WalletWebMode Application
    application = do
        let app = Servant.serve API.walletDocAPI LegacyServer.walletDocServer
        return $ withMiddleware walletRunMode app
    tls =
        if isDebugMode walletRunMode then Nothing else walletTLSParams

-- | A @Plugin@ to start the wallet backend API.
legacyWalletBackend :: (HasConfigurations, HasCompileInfo)
                    => ProtocolMagic
                    -> TxpConfiguration
                    -> WalletBackendParams
                    -> TVar NtpStatus
                    -> Plugin WalletWebMode
legacyWalletBackend pm txpConfig WalletBackendParams {..} ntpStatus = pure $ \diffusion -> do
    modifyLoggerName (const "legacyServantBackend") $ do
      logInfo $ sformat ("Production mode for API: "%build)
        walletProductionApi
      logInfo $ sformat ("Transaction submission disabled: "%build)
        walletTxCreationDisabled

      ctx <- view shutdownContext
      let
        portCallback :: Word16 -> IO ()
        portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
      walletServeImpl
        (getApplication diffusion)
        walletAddress
        -- Disable TLS if in debug mode.
        (if isDebugMode walletRunMode then Nothing else walletTLSParams)
        (Just $ setOnExceptionResponse exceptionHandler defaultSettings)
        (Just portCallback)
  where
    -- Gets the Wai `Application` to run.
    getApplication :: Diffusion WalletWebMode -> WalletWebMode Application
    getApplication diffusion = do
        logInfo "Wallet Web API has STARTED!"
        wsConn <- getWalletWebSockets
        ctx <- V0.walletWebModeContext
        return
            $ withMiddleware walletRunMode
            $ upgradeApplicationWS wsConn
            $ Servant.serve API.walletAPI
            $ LegacyServer.walletServer
                (V0.convertHandler ctx)
                pm
                txpConfig
                diffusion
                ntpStatus
                walletRunMode

    exceptionHandler :: SomeException -> Response
    exceptionHandler se =
        case asum [handleV1Errors se, handleV0Errors se] of
             Nothing -> handleGenericError se
             Just r  -> r

    -- Handles domain-specific errors coming from the V1 API.
    handleV1Errors :: SomeException -> Maybe Response
    handleV1Errors se =
        let reify (we :: V1.WalletError) =
                responseLBS (V1.toHttpErrorStatus we) [applicationJson] .  encode $ we
        in fmap reify (fromException se)

    -- Handles domain-specific errors coming from the V0 API, but rewraps it
    -- into a jsend payload. It doesn't explicitly handle 'InternalError' or
    -- 'DecodeError', as they can come from any part of the stack or even
    -- rewrap some other exceptions (cfr 'rewrapToWalletError').
    -- Uses the 'Buildable' istance on 'WalletError' to exploit any
    -- available rendering and information-masking improvements.
    handleV0Errors :: SomeException -> Maybe Response
    handleV0Errors se =
        let maskSensitive err =
                case err of
                    V0.RequestError _  -> err
                    V0.InternalError _ -> V0.RequestError "InternalError"
                    V0.DecodeError _   -> V0.RequestError "DecodeError"
            reify :: V0.WalletError -> V1.WalletError
            reify = V1.UnknownError . sformat build . maskSensitive
        in fmap (responseLBS badRequest400 [applicationJson] .  encode . reify) (fromException se)

    -- Handles any generic error, trying to prevent internal exceptions from leak outside.
    handleGenericError :: SomeException -> Response
    handleGenericError _ =
        let
            unknownV1Error = V1.UnknownError "Something went wrong."
        in
            responseLBS badRequest400 [applicationJson] $ encode unknownV1Error


-- | A 'Plugin' to start the wallet REST server
--
-- NOTE: There is no web socket support in the new wallet for now.
walletBackend :: ProtocolMagic
              -> NewWalletBackendParams
              -> (PassiveWalletLayer IO, PassiveWallet)
              -> Plugin Kernel.WalletMode
walletBackend protocolMagic (NewWalletBackendParams WalletBackendParams{..}) (passiveLayer, passiveWallet) =
    pure $ \diffusion -> do
        env <- ask
        let diffusion' = Kernel.fromDiffusion (lower env) diffusion
        WalletLayer.Kernel.bracketActiveWallet protocolMagic passiveLayer passiveWallet diffusion' $ \active _ -> do
          ctx <- view shutdownContext
          let
            portCallback :: Word16 -> IO ()
            portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
          walletServeImpl
            (getApplication active)
            walletAddress
            -- Disable TLS if in debug modeit .
            (if isDebugMode walletRunMode then Nothing else walletTLSParams)
            Nothing
            (Just portCallback)
  where
    getApplication :: ActiveWalletLayer IO -> Kernel.WalletMode Application
    getApplication active = do
      logInfo "New wallet API has STARTED!"
      return $ withMiddleware walletRunMode $
        Servant.serve API.walletAPI $ Server.walletServer active walletRunMode

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

-- | A @Plugin@ to resubmit pending transactions.
resubmitterPlugin :: HasConfigurations
                  => ProtocolMagic
                  -> TxpConfiguration
                  -> Plugin WalletWebMode
resubmitterPlugin pm txpConfig = [\diffusion -> askWalletDB >>= \db ->
                        startPendingTxsResubmitter pm txpConfig db (sendTx diffusion)]

-- | A @Plugin@ to notify frontend via websockets.
notifierPlugin :: HasConfigurations => Plugin WalletWebMode
notifierPlugin = [const V0.notifierPlugin]

-- | The @Plugin@ responsible for the restoration & syncing of a wallet.
syncWalletWorker :: HasConfigurations => Plugin WalletWebMode
syncWalletWorker = pure $ const $
    modifyLoggerName (const "syncWalletWorker") $
    (view (lensOf @SyncQueue) >>= processSyncRequest)

-- | "Attaches" the middleware to this 'Application', if any.
-- When running in debug mode, chances are we want to at least allow CORS to test the API
-- with a Swagger editor, locally.
withMiddleware :: RunMode -> Application -> Application
withMiddleware wrm app
  | isDebugMode wrm = corsMiddleware (rejectHTTP20 app)
  | otherwise = rejectHTTP20 app

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods
        }


-- | On some requests, when using HTTP/2.0, particularly those involving
-- streaming, the current API -- drops the connection with a rather unclear
-- error message. There's no particular intent to support HTTP/2.0, so we'll
-- simply clearly forbid it for now.
rejectHTTP20 :: Middleware
rejectHTTP20 =
    ifRequest (\req -> httpVersion req >= http20) (modifyResponse rejectResponse)
  where
    rejectResponse :: Response -> Response
    rejectResponse res = do
        let msg =
              "HTTP/2.0 protocol isn't yet supported by the API. Please configure your client to rely on HTTP/1.1 instead."

        let body =
              encode . NotSupportedError $ msg

        responseLBS httpVersionNotSupported505 (applicationJson : responseHeaders res) body


newtype NotSupportedError
    = NotSupportedError Text
    deriving (Generic, Eq, Show)

deriveGeneric ''NotSupportedError

instance ToJSON NotSupportedError where
    toEncoding (NotSupportedError weDescription) = pairs $ mconcat
        [ "message"    .= String "NotSupportedError"
        , "status"     .= String "error"
        , "diagnostic" .= object
            [ "description" .= weDescription
            ]
        ]

instance FromJSON NotSupportedError where
    parseJSON (Object o)
        | HMS.member "message" o =
            case HMS.lookup "message" o of
                Just "NotSupportedError" ->
                    NotSupportedError <$> ((o .: "diagnostic") >>= (.: "description"))
                _ ->
                    fail "Incorrect JSON encoding for NotSupportedError"

        | otherwise =
            fail "Incorrect JSON encoding for NotSupportedError"

    parseJSON invalid =
        typeMismatch "NotSupportedError" invalid
