module Cardano.Wallet.Kernel.Restore
    ( restoreWalletBalance
    , restoreWalletHistory
    ) where

import           Universum

import           Data.Acid (update)
import           Pos.Core.Txp (toaOut, txOutAddress)

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
                     (UpdateCurrentCheckpointUtxo (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.Internal (walletNode, wallets)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (withNodeState)

import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentials,
                     decryptAddress)
import           Pos.Chain.Txp (getTotalCoinsInUtxo, utxoToModifier)
import           Pos.Core (Coin)
import           Pos.DB.Txp.Utxo (filterUtxo)

restoreWalletBalance :: Kernel.PassiveWallet -> WalletDecrCredentials -> IO Coin
restoreWalletBalance wallet wdc = do
    utxo <- withNodeState (wallet ^. walletNode) (\_lock -> filterUtxo mine)
    update (wallet ^. wallets) (UpdateCurrentCheckpointUtxo (utxoToModifier utxo))
    return (getTotalCoinsInUtxo utxo)
  where
    mine = isJust . decryptAddress wdc . txOutAddress . toaOut . snd


restoreWalletHistory :: Kernel.PassiveWallet -> HD.HdRoot -> IO ()
restoreWalletHistory _wallet _hdRoot =
    putStrLn ("MN TODO: restoreWalletHistory" :: Text)
