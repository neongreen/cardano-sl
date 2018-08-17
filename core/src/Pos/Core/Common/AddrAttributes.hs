{-# LANGUAGE DeriveGeneric #-}

module Pos.Core.Common.AddrAttributes
       ( AddrAttributes (..)
       , NetworkMagic (..)
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import           Data.Serialize (getWord8, putWord8)
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi, decode, encode)
import qualified Pos.Binary.Class as Bi
import           Pos.Core.Attributes (Attributes (..), decodeAttributes,
                     encodeAttributes)
import           Pos.Core.Common.AddrStakeDistribution
import           Pos.Crypto.HD (HDAddressPayload)
import           Pos.Util.Util (cerealError)

data NetworkMagic
    = NMNothing
    | NMJust !Int32
    deriving (Eq, Generic, Ord, Show)

instance NFData NetworkMagic

instance SafeCopy NetworkMagic where
    getCopy = contain $ getWord8 >>= \case
        0 -> pure NMNothing
        1 -> NMJust <$> safeGet
        t -> cerealError $ "getCopy@NetworkMagic: couldn't read tag: " <> show t
    putCopy NMNothing  = contain $ putWord8 0
    putCopy (NMJust x) = contain $ putWord8 1 >> safePut x

-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
data AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: !(Maybe HDAddressPayload)
    , aaStakeDistribution :: !AddrStakeDistribution
    -- TODO mhueschen - turn this into an Int8 or Word8 to be smaller
    , aaNetworkMagic      :: !NetworkMagic
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable AddrAttributes where
    build (AddrAttributes {..}) =
        bprint
            ("AddrAttributes { stake distribution: "%build%
             ", derivation path: "%builder%" }")
            aaStakeDistribution
            derivationPathBuilder
      where
        derivationPathBuilder =
            case aaPkDerivationPath of
                Nothing -> "{}"
                Just _  -> "{path is encrypted}"

instance NFData AddrAttributes

{- NOTE: Address attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attributes' are conceptually a map, where keys are numbers ('Word8').

For address there are two attributes:
• 0 — stake distribution, defaults to 'BootstrapEraDistr';
• 1 — derivation path, defaults to 'Nothing'.

-}

instance Bi (Attributes AddrAttributes) where
    -- FIXME @avieth it was observed that for a 150kb block, this call to
    -- encodeAttributes allocated 3.685mb
    -- Try using serialize rather than serialize', to avoid the
    -- toStrict call.
    -- Also consider using a custom builder strategy; serialized attributes are
    -- probably small, right?
    encode attrs@(Attributes {attrData = AddrAttributes derivationPath stakeDistr networkMagic}) =
        encodeAttributes listWithIndices attrs
      where
        listWithIndices :: [(Word8, AddrAttributes -> LBS.ByteString)]
        listWithIndices = stakeDistributionListWithIndices
                       <> derivationPathListWithIndices
                       <> networkMagicListWithIndices

        stakeDistributionListWithIndices =
            case stakeDistr of
                BootstrapEraDistr -> []
                _                 -> [(0, Bi.serialize . aaStakeDistribution)]
        derivationPathListWithIndices =
            case derivationPath of
                Nothing -> []
                -- 'unsafeFromJust' is safe, because 'case' ensures
                -- that derivation path is 'Just'.
                Just _ ->
                    [(1, Bi.serialize . unsafeFromJust . aaPkDerivationPath)]

        networkMagicListWithIndices =
            case networkMagic of
                NMNothing -> []
                NMJust x  ->
                    [(2, \_ -> Bi.serialize x)]

        unsafeFromJust =
            fromMaybe
                (error "Maybe was Nothing in Bi (Attributes AddrAttributes)")

    decode = decodeAttributes initValue go
      where
        initValue =
            AddrAttributes
            { aaPkDerivationPath = Nothing
            , aaStakeDistribution = BootstrapEraDistr
            , aaNetworkMagic = NMNothing
            }
        go n v acc =
            case n of
                0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> Bi.deserialize v
                1 -> (\deriv -> Just $ acc {aaPkDerivationPath = Just deriv }) <$> Bi.deserialize v
                2 -> (\deriv -> Just $ acc {aaNetworkMagic = NMJust deriv }    ) <$> Bi.deserialize v
                _ -> pure Nothing

deriveSafeCopySimple 0 'base ''AddrAttributes
