{-# LANGUAGE RankNTypes #-}

-- | Configuration for the txp package.

module Pos.Chain.Txp.Configuration
       ( TxpConfiguration(..)
       , RequiresNetworkMagic (..)
       , memPoolLimitTx
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON, withText)
import           Data.Aeson.Options (defaultOptions)

import           Pos.Core (Address)
import           Pos.Util.Util (toAesonError)

data RequiresNetworkMagic
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Eq, Generic, Show)

instance ToJSON RequiresNetworkMagic where
    toJSON NMMustBeNothing = toJSON False
    toJSON NMMustBeJust    = toJSON True

instance FromJSON RequiresNetworkMagic where
    parseJSON = withText "RequiresNetworkMagic" $ toAesonError . \case
        "False" -> Right NMMustBeNothing
        "True"  -> Right NMMustBeJust
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are True|False")

-- | Delegation configruation part.
data TxpConfiguration = TxpConfiguration
    { -- | Limit on the number of transactions that can be stored in
      -- the mem pool.
      ccMemPoolLimitTx       :: !Int

      -- | Set of source address which are asset-locked. Transactions which
      -- use these addresses as transaction inputs will be silently dropped.
    , tcAssetLockedSrcAddrs  :: !(Set Address)

      -- | Field indicating presence/absense of ProtocolMagic in Addresses
      -- for this cluster.
    , tcRequiresNetworkMagic :: !RequiresNetworkMagic
    } deriving (Eq, Show, Generic)

instance ToJSON TxpConfiguration where
    toJSON = genericToJSON defaultOptions

instance FromJSON TxpConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Limint on the number of transactions that can be stored in
-- the mem pool.
memPoolLimitTx :: Integral i => TxpConfiguration -> i
memPoolLimitTx txpConfig = fromIntegral . ccMemPoolLimitTx $ txpConfig
