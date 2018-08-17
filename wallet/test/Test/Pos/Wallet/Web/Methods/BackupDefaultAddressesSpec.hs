{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Monadic (pick)

import           Pos.Chain.Txp (RequiresNetworkMagic (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Wallet.Web.ClientTypes (CWallet (..))
import           Pos.Wallet.Web.Methods.Restore (restoreWalletFromBackup)

import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = do
    runWithNetworkMagic NMMustBeJust
    runWithNetworkMagic NMMustBeNothing

runWithNetworkMagic :: RequiresNetworkMagic -> Spec
runWithNetworkMagic requiresNetworkMagic = do
    withDefConfigurations requiresNetworkMagic $ \_ _ _ ->
        describe ("restoreAddressFromWalletBackup (requiresNetworkMagic="
                      <> show requiresNetworkMagic
                      <> ")") $ modifyMaxSuccess (const 10) $ do
            restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: HasConfigurations => Spec
restoreWalletAddressFromBackupSpec =
    walletPropertySpec restoreWalletAddressFromBackupDesc $ do
        walletBackup   <- pick arbitrary
        restoredWallet <- lift $ restoreWalletFromBackup walletBackup
        let noOfAccounts = cwAccountsNumber restoredWallet
        assertProperty (noOfAccounts > 0) $ "Exported wallet has no accounts!"
  where
    restoreWalletAddressFromBackupDesc =
        "Generate wallet backup; "
            <> "Restore it; "
            <> "Check if the wallet has some accounts; "
