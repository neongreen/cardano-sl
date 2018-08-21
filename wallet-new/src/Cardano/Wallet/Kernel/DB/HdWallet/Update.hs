-- | UPDATE operations on HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet.Update (
    updateHdRoot
  , updateHdRootPassword
  , updateHdAccountName
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.Util (modifyAndGetNew)

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

-- | Updates in one gulp the Hd Wallet name and assurance level.
updateHdRoot :: HdRootId
             -> AssuranceLevel
             -> WalletName
             -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRoot rootId assurance name =
    zoomHdRootId identity rootId $ do
        modifyAndGetNew $ set hdRootAssurance assurance . set hdRootName name

updateHdRootPassword :: HdRootId
                     -> HasSpendingPassword
                     -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootPassword rootId hasSpendingPassword =
    zoomHdRootId identity rootId $ do
        modifyAndGetNew $ hdRootHasPassword .~ hasSpendingPassword

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        modifyAndGetNew $ hdAccountName .~ name
