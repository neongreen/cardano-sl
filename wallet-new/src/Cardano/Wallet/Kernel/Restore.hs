module Cardano.Wallet.Kernel.Restore
    ( beginWalletRestoration
    , restoreWalletBalance
    , restoreWalletHistory
    ) where

import           Universum

import           Control.Concurrent.Async (async, link)
import           Data.Acid (update)

import           Cardano.Wallet.API.V1.Types (SyncProgress (..),
                     mkEstimatedCompletionTime, mkSyncPercentage,
                     mkSyncThroughput)
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
                     (UpdateCurrentCheckpointUtxo (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentials,
                     decryptAddress)
import           Cardano.Wallet.Kernel.Internal (walletNode, wallets)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (withNodeState)

import           Pos.Chain.Txp (getTotalCoinsInUtxo, utxoToModifier)
import           Pos.Core (Coin)
import           Pos.Core.Txp (toaOut, txOutAddress)
import           Pos.DB.Txp.Utxo (filterUtxo)

-- | Begin a wallet restoration by setting the sync state of all wallet accounts.
beginWalletRestoration :: Kernel.PassiveWallet -> IO ()
beginWalletRestoration _wallet = return ()

-- | Scan the node's current UTXO set for any that belong to this wallet. Use them
--   to update the current checkpoint's UTXO set, and return the total 'Coin' value
--   of the UTXO belonging to this wallet.
restoreWalletBalance :: Kernel.PassiveWallet -> WalletDecrCredentials -> IO Coin
restoreWalletBalance wallet wdc = do
    utxo <- withNodeState (wallet ^. walletNode) (\_lock -> filterUtxo mine)
    update (wallet ^. wallets) (UpdateCurrentCheckpointUtxo (utxoToModifier utxo))
    return (getTotalCoinsInUtxo utxo)
  where
    mine = isJust . decryptAddress wdc . txOutAddress . toaOut . snd

-- | Start restoring the wallet history in the background, returning a best-effort
--   estimate of the initial 'SyncProgress'.
restoreWalletHistory :: Kernel.PassiveWallet -> HD.HdRoot -> IO SyncProgress
restoreWalletHistory wallet hdRoot = do
  link =<< async (restoreWalletHistoryAsync wallet hdRoot)
  return SyncProgress {
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
