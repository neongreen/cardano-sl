-- | Resolved blocks and transactions
module Cardano.Wallet.Kernel.DB.Resolved (
    -- * Resolved blocks and transactions
    ResolvedInput
  , ResolvedTx(..)
  , ResolvedBlock(..)
    -- * MetaData
  , toTxMeta
    -- ** Lenses
  , rtxInputs
  , rtxOutputs
  , rtxMeta
  , rbTxs
  , rbSlotId
  , rbMeta
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Formatting (bprint, (%))
import           Formatting.Buildable

import           Serokell.Util (listJson, mapJson, pairF)

import qualified Pos.Chain.Txp as Core
import           Pos.Core (SlotId, Timestamp)

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = Core.TxOutAux

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      _rtxInputs  :: InDb (NonEmpty (Core.TxIn, ResolvedInput))

      -- | Transaction outputs
    , _rtxOutputs :: InDb Core.Utxo

     -- | Transaction Meta
    , _rtxMeta    :: InDb Meta
    }

type Meta = (Core.TxId, Timestamp)

toTxMeta :: ResolvedTx -> HD.HdAccountId -> TxMeta
toTxMeta ResolvedTx{..} accountId =
  fromJust mbMeta
  where
    mbMeta = do
      inps <- NE.nonEmpty $ mapMaybe toInpQuad $ NE.toList (_fromDb _rtxInputs)
      outs <- fromUtxo $ _fromDb _rtxOutputs
      let (txId, timestamp) = _fromDb _rtxMeta
      return TxMeta {
          _txMetaId = txId
        , _txMetaAmount = minBound
        , _txMetaInputs = inps
        , _txMetaOutputs = outs
        , _txMetaCreationAt = timestamp
        , _txMetaIsLocal = False
        , _txMetaIsOutgoing = False
        , _txMetaWalletId = _fromDb $ HD.getHdRootId (accountId ^. HD.hdAccountIdParent)
        , _txMetaAccountIx = HD.getHdAccountIx $ accountId ^. HD.hdAccountIdIx
      }

    toInpQuad (txIn, resolvedInput) = do
      (txId, ix) <- derefIn txIn
      let (addr, coin) = toOutPair resolvedInput
      return (txId, ix, addr, coin)


-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs    :: ![ResolvedTx]

      -- | Slot ID of this block
    , _rbSlotId :: !SlotId

      -- | Creation time of this block
    , _rbMeta   :: !Timestamp
    }

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ResolvedTx where
  build ResolvedTx{..} = bprint
    ( "ResolvedTx "
    % "{ inputs:  " % mapJson
    % ", outputs: " % mapJson
    % ", meta:    " % pairF
    % "}"
    )
    (Map.fromList (toList (_rtxInputs  ^. fromDb)))
    (_rtxOutputs ^. fromDb)
    (_rtxMeta ^. fromDb)

instance Buildable ResolvedBlock where
  build ResolvedBlock{..} = bprint
    ( "ResolvedBlock "
    % "{ txs: " % listJson
    % "}"
    )
    _rbTxs
