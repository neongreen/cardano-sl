{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Chain.Txp.Arbitrary
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..), oneof)

import           Pos.Chain.Txp (RequiresNetworkMagic (..))

instance Arbitrary RequiresNetworkMagic where
    arbitrary =
        oneof
            [ pure NMMustBeNothing
            , pure NMMustBeJust
            ]
