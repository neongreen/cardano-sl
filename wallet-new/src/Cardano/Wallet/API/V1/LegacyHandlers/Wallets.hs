module Cardano.Wallet.API.V1.LegacyHandlers.Wallets (
      handlers

    -- * Internals, exposed only for testing
    , isNodeSufficientlySynced
    , newWallet
    ) where

import           Universum
import           UnliftIO (MonadUnliftIO)

import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods as V0
import qualified Pos.Wallet.Web.State as V0 (WalletSnapshot, askWalletSnapshot)
import qualified Pos.Wallet.Web.State.Storage as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.V1.Wallets as Wallets
import qualified Data.IxSet.Typed as IxSet
import qualified Pos.Core as Core
import           Pos.Update.Configuration ()

import           Pos.Util (HasLens (..))
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.Error.Types as V0
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic,
                     MonadWalletLogicRead)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Servant

import qualified Control.Foldl as L
import           Data.HashMap.Strict as HMS
import qualified Data.Text as T

-- | All the @Servant@ handlers for wallet-specific operations.
handlers :: HasConfigurations
         => ServerT Wallets.API MonadV1
handlers = newWallet
    :<|> listWallets
    :<|> updatePassword
    :<|> deleteWallet
    :<|> getWallet
    :<|> updateWallet
    :<|> getUtxoStatistics


-- | Pure function which returns whether or not the underlying node is
-- \"synced enough\" to allow wallet creation/restoration. The notion of
-- \"synced enough\" is quite vague and if made too stringent could prevent
-- the backend to operate normally for wallets which are on a slow network
-- or are struggling to keep up. Therefore we consider a node to be \"synced
-- enough\" with the blockchain if we are not lagging more than @k@ slots, where
-- @k@ comes from the 'blkSecurityParam'.
isNodeSufficientlySynced :: Core.HasProtocolConstants => V0.SyncProgress -> Bool
isNodeSufficientlySynced spV0 =
    let blockchainHeight = fromMaybe (Core.BlockCount maxBound)
                                     (Core.getChainDifficulty <$> V0._spNetworkCD spV0)
        localHeight = Core.getChainDifficulty . V0._spLocalCD $ spV0
        remainingBlocks = blockchainHeight - localHeight

        in remainingBlocks <= Core.blkSecurityParam

-- | Creates a new or restores an existing @wallet@ given a 'NewWallet' payload.
-- Returns to the client the representation of the created or restored
-- wallet in the 'Wallet' type.
newWallet
    :: (MonadThrow m
       , MonadUnliftIO m
       , MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       , HasLens SyncQueue ctx SyncQueue
       )
    => NewWallet
    -> m (WalletResponse Wallet)
newWallet NewWallet{..} = do

    spV0 <- V0.syncProgress
    syncPercentage <- migrate spV0

    -- Do not allow creation or restoration of wallets if the underlying node
    -- is still catching up.
    unless (isNodeSufficientlySynced spV0) $ throwM (NodeIsStillSyncing syncPercentage)

    let newWalletHandler CreateWallet  = V0.newWallet
        newWalletHandler RestoreWallet = V0.restoreWalletFromSeed
        (V1 spendingPassword) = fromMaybe (V1 mempty) newwalSpendingPassword
        (V1 backupPhrase) = newwalBackupPhrase
    initMeta <- V0.CWalletMeta <$> pure newwalName
                              <*> migrate newwalAssuranceLevel
                              <*> pure 0
    let walletInit = V0.CWalletInit initMeta (V0.CBackupPhrase backupPhrase)
    single <$> do
        v0wallet <- newWalletHandler newwalOperation spendingPassword walletInit
                        `catch` rethrowDuplicateMnemonic
        ss <- V0.askWalletSnapshot
        addWalletInfo ss v0wallet
  where
    -- NOTE: this is temporary solution until we get rid of V0 error handling and/or we lift error handling into types:
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183469153
    --   https://github.com/input-output-hk/cardano-sl/pull/2811#discussion_r183472103
    rethrowDuplicateMnemonic (e :: V0.WalletError) =
        case e of
            V0.RequestError "Wallet with that mnemonics already exists" -> throwM WalletAlreadyExists
            _ -> throwM e

-- | Returns the full (paginated) list of wallets.
listWallets :: ( MonadThrow m
               , V0.MonadWalletLogicRead ctx m
               , V0.MonadBlockchainInfo m
               )
            => RequestParams
            -> FilterOperations Wallet
            -> SortOperations Wallet
            -> m (WalletResponse [Wallet])
listWallets params fops sops = do
    ws <- V0.askWalletSnapshot
    currentDepth <- V0.networkChainDifficulty
    respondWith params fops sops (IxSet.fromList <$> do
        (V0.getWalletsWithInfo ws >>= (migrate @_ @[V1.Wallet] . Universum.map (\(w, i) -> (w,i,currentDepth)))))

updatePassword
    :: ( MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId -> PasswordUpdate -> m (WalletResponse Wallet)
updatePassword wid PasswordUpdate{..} = do
    wid' <- migrate wid
    let (V1 old) = pwdOld
        (V1 new) = pwdNew
    _ <- V0.changeWalletPassphrase wid' old new
    single <$> do
        ss <- V0.askWalletSnapshot
        wallet <- V0.getWallet wid'
        addWalletInfo ss wallet

-- | Deletes an exisiting wallet.
deleteWallet
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m NoContent
deleteWallet = V0.deleteWallet <=< migrate

getWallet :: ( MonadThrow m
             , MonadWalletLogicRead ctx m
             , V0.MonadBlockchainInfo m
             ) => WalletId -> m (WalletResponse Wallet)
getWallet wid = do
    ss <- V0.askWalletSnapshot
    wid' <- migrate wid
    wallet <- V0.getWallet wid'
    single <$> addWalletInfo ss wallet

addWalletInfo
    :: ( MonadThrow m
       , V0.MonadWalletLogicRead ctx m
       , V0.MonadBlockchainInfo m
       )
    => V0.WalletSnapshot
    -> V0.CWallet
    -> m Wallet
addWalletInfo snapshot wallet = do
    case V0.getWalletInfo (V0.cwId wallet) snapshot of
        Nothing ->
            throwM WalletNotFound
        Just walletInfo -> do
            currentDepth <- V0.networkChainDifficulty
            migrate (wallet, walletInfo, currentDepth)

updateWallet
    :: (V0.MonadWalletLogic ctx m
       , V0.MonadBlockchainInfo m
       )
    => WalletId
    -> WalletUpdate
    -> m (WalletResponse Wallet)
updateWallet wid WalletUpdate{..} = do
    ws <- V0.askWalletSnapshot
    wid' <- migrate wid
    assurance <- migrate uwalAssuranceLevel
    walletMeta <- maybe (throwM WalletNotFound) pure $ V0.getWalletMeta wid' ws
    updated <- V0.updateWallet wid' walletMeta
        { V0.cwName = uwalName
        , V0.cwAssurance = assurance
        }
    single <$> do
        -- reacquire the snapshot because we did an update
        ws' <- V0.askWalletSnapshot
        addWalletInfo ws' updated

-- | Gets Utxo statistics for a wallet.
-- | Now stub, not calling data layer yet.
getUtxoStatistics
    :: (MonadWalletLogic ctx m)
    => WalletId
    -> m (WalletResponse UtxoStatistics)
getUtxoStatistics wid = do
    return $ single (computeUtxoStatistics [1::Integer,2,3,10,20,30,101])


computeUtxoStatistics :: [Integer] ->  UtxoStatistics
computeUtxoStatistics xs = L.fold (summarizeUtxoStatistics $ generateBounds Log10) xs

-- | Using foldl library enable as to capture a number of aggregations in one pass. This thanks to L.Fold being an Applicative
summarizeUtxoStatistics :: [Integer] -> L.Fold Integer UtxoStatistics
summarizeUtxoStatistics bounds = UtxoStatistics
  <$> populateBuckets bounds
  <*> L.sum

-- | Buckets boundaries can be constructed in different way
data BoundType = Log10 | Haphazard

generateBounds :: BoundType -> [Integer]
generateBounds bType =
    case bType of
        Log10 -> (zipWith (\ten toPower -> ten^toPower :: Integer) (repeat (10::Integer)) [(1::Integer)..16]) ++ [45 * (10^(15::Integer))]
        Haphazard -> [10, 100, 1000, 10000]


populateBuckets ::  [Integer] ->  L.Fold Integer [HistogramBar]
populateBuckets bounds =
    case bounds of
        (x:_) -> L.Fold (addCountInBuckets x) (initalizeHM bounds) (fmap (\pair -> HistogramBar (T.pack $ show $ fst pair, snd pair) ) . HMS.toList)
        _ -> error "populateBuckets has to be powered with nonempty bounds"
    where
        initalizeHM :: [Integer] -> HMS.HashMap Integer Integer
        initalizeHM b = HMS.fromList $ zip b (repeat 0)
        retrieveProperBound :: (Ord a) => [a] -> a -> a -> a
        retrieveProperBound [] _ prev = prev
        retrieveProperBound (x:xs) stake _ =
            if (stake > x) then
                retrieveProperBound xs stake x
            else
                x
        addCountInBuckets :: Integer -> HMS.HashMap Integer Integer -> Integer -> HMS.HashMap Integer Integer
        addCountInBuckets firstElem acc entry = HMS.adjust (+1) (retrieveProperBound bounds entry firstElem) acc
