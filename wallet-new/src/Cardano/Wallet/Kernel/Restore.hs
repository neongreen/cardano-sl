module Cardano.Wallet.Kernel.Restore
    ( beginWalletRestoration
    , restoreWalletBalance
    , restoreWalletHistory
    ) where

import           Universum

import           Control.Concurrent.Async (async, link)
import           Data.Acid (update)
import           Data.Conduit (mapOutputMaybe, runConduitRes, (.|))
import qualified Data.Conduit.List as Conduit
import qualified Data.Map as M
import           UnliftIO (MonadUnliftIO)

import           Cardano.Wallet.API.V1.Types (SyncProgress (..), SyncState (..),
                     mkEstimatedCompletionTime, mkSyncPercentage,
                     mkSyncThroughput)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress (..),
                     UpdateCurrentCheckpointUtxo (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdAddress)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Util.AcidState (Update', runUpdate',
                     runUpdateDiscardSnapshot, zoomAll)
import           Cardano.Wallet.Kernel.Decrypt (decryptAddress)
import           Cardano.Wallet.Kernel.Internal (walletNode, wallets)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (withNodeState)
import           Cardano.Wallet.Kernel.PrefilterTx (WalletKey, toHdAddressId)

import           Pos.Chain.Txp (Utxo, getTotalCoinsInUtxo, utxoToModifier)
import           Pos.Core (Address, Coin)
import           Pos.Core.Txp (toaOut, txOutAddress)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Txp.Utxo (utxoSource)
import           Pos.Wallet.Web.State (WAddressMeta (..))

-- | Begin a wallet restoration by setting the sync state of all wallet accounts.
beginWalletRestoration :: Kernel.PassiveWallet -> IO ()
beginWalletRestoration wallet = do
    -- Set the state of each account in this wallet to "more than k slots from the tip"
    -- TODO (@mn): is zoomAll ok here? The comment says it must reconstruct the whole
    --             IxSet, so should only be used for "small" sets.
    let db = wallet ^. wallets
    update db
      $ runUpdateDiscardSnapshot
      $ zoomAll (dbHdWallets ^. HD.hdWalletsAccounts) setAcctState
    return ()
  where

    setAcctState :: Update' HD.HdAccountState Void ()
    setAcctState =
      hdAccountState .= HD.HdAccountOutsideK { _hdOutsideKCurrent = error "TODO"
                                             , _hdOutsideKHistorical = error "TODO"
                                             }
-- | Scan the node's current UTXO set for any that belong to this wallet. Use them
--   to update the current checkpoint's UTXO set, and return the total 'Coin' value
--   of the UTXO belonging to this wallet.
restoreWalletBalance :: Kernel.PassiveWallet -> WalletKey -> IO Coin
restoreWalletBalance wallet (wId, wdc) = do
    -- Find all of the current UTXO that this wallet owns.
    (utxo, addrs) <- withNodeState (wallet ^. walletNode) (\_lock -> filterMyUtxo)

    -- Create the addresses referenced by this wallet's UTXO.
    let db = wallet ^. wallets
    forM_ addrs $ \(a,wam) ->
        update db (CreateHdAddress (initHdAddress (toHdAddressId wId wam) (InDb a)))

    -- Update the current checkpoint with the newly-found UTXO.
    update db (UpdateCurrentCheckpointUtxo (utxoToModifier utxo))

    -- Return the wallet's current balance.
    return (getTotalCoinsInUtxo utxo)
  where
    txoAddr = txOutAddress . toaOut . snd
    filterMyUtxo :: (MonadDBRead m, MonadUnliftIO m) => m (Utxo, [(Address, WAddressMeta)])
    filterMyUtxo = runConduitRes
         $ mapOutputMaybe (\u -> let a = txoAddr u in (u,a,) <$> decryptAddress wdc a) utxoSource
        .| Conduit.fold (\(m,as) ((k,v), a, w) -> (M.insert k v m, (a,w) : as)) (mempty, [])

-- | Start restoring the wallet history in the background, returning a best-effort
--   estimate of the initial 'SyncProgress'.
restoreWalletHistory :: Kernel.PassiveWallet -> HD.HdRoot -> IO SyncState
restoreWalletHistory wallet hdRoot = do
  link =<< async (restoreWalletHistoryAsync wallet hdRoot)
  return $ Restoring $ SyncProgress {
        spEstimatedCompletionTime = mkEstimatedCompletionTime 0
      , spThroughput              = mkSyncThroughput 0
      , spPercentage              = mkSyncPercentage 0
      }

-- | Restore a wallet's transaction history.
restoreWalletHistoryAsync :: Kernel.PassiveWallet -> HD.HdRoot -> IO ()
restoreWalletHistoryAsync _wallet _hdRoot = do
    -- 1. Compute PartialCheckpoints using data from the node.
    -- 2.
    return ()
