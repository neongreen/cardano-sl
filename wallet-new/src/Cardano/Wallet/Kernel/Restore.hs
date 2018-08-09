module Cardano.Wallet.Kernel.Restore
    ( restoreWalletBalance
    , restoreWalletHistory
    ) where

import           Universum

import           Pos.Core.Txp (toaOut, txOutAddress)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.Internal (walletNode)
import           Cardano.Wallet.Kernel.NodeStateAdaptor (withNodeState)

import           Cardano.Wallet.Kernel.Decrypt (decryptAddress)
import           Cardano.Wallet.Kernel.PrefilterTx (WalletKey)
import           Pos.DB.Txp.Utxo (filterUtxo)

restoreWalletBalance :: Kernel.PassiveWallet -> HD.HdRoot -> WalletKey -> IO ()
restoreWalletBalance wallet _hdRoot (_wId, wdc) =
    withNodeState (wallet ^. walletNode) $ \_lock -> do
        _utxo <- filterUtxo isWalletUtxo
        return ()

  where
      isWalletUtxo = isJust . decryptAddress wdc . txOutAddress . toaOut . snd

restoreWalletHistory :: Kernel.PassiveWallet -> HD.HdRoot -> IO ()
restoreWalletHistory _wallet _hdRoot =
    putStrLn ("MN TODO: restoreWalletHistory" :: Text)
