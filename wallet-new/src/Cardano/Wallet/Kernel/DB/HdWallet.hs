{-# LANGUAGE RankNTypes #-}
-- TODO: Not sure about the best way to avoid the orphan instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet (
    -- * Supporting types
    WalletName(..)
  , AccountName(..)
  , HdAccountIx(..)
  , HdAddressIx(..)
  , AssuranceLevel(..)
  , assuredBlockDepth
  , HasSpendingPassword(..)
    -- * HD wallet types proper
  , HdWallets(..)
  , HdRootId(..)
  , HdAccountId(..)
  , HdAddressId(..)
  , HdRoot(..)
  , HdAccount(..)
  , HdAddress(..)
    -- * HD Wallet state
  , HdAccountState(..)
  , HdAccountUpToDate(..)
  , HdAccountWithinK(..)
  , HdAccountOutsideK(..)
  , reachedWithinK
  , finishRestoration
    -- ** Initialiser
  , initHdWallets
    -- ** Lenses
    -- *** Wallet collection
  , hdWalletsRoots
  , hdWalletsAccounts
  , hdWalletsAddresses
    -- *** Account ID
  , hdAccountIdParent
  , hdAccountIdIx
  , hdAccountAutoPkCounter
    -- ** Address ID
  , hdAddressIdParent
  , hdAddressIdIx
    -- *** Root
  , hdRootId
  , hdRootName
  , hdRootHasPassword
  , hdRootAssurance
  , hdRootCreatedAt
    -- *** Account
  , hdAccountId
  , hdAccountName
  , hdAccountState
  , hdAccountStateCurrent
    -- *** Account state: up to date
  , hdUpToDateCheckpoints
    -- *** Account state: within K slots
  , hdWithinKCurrent
  , hdWithinKHistorical
    -- *** Account state: outside K slots
  , hdOutsideKCurrent
  , hdOutsideKHistorical
    -- *** Address
  , hdAddressId
  , hdAddressAddress
    -- ** Composite lenses
  , hdAccountRootId
  , hdAddressRootId
  , hdAddressAccountId
    -- * Unknown identifiers
  , UnknownHdRoot(..)
  , UnknownHdAccount(..)
  , UnknownHdAddress(..)
  , embedUnknownHdRoot
  , embedUnknownHdAccount
    -- * Zoom to parts of the HD wallet
  , zoomHdRootId
  , zoomHdAccountId
  , zoomHdAddressId
  , zoomHdCardanoAddress
  , matchHdAccountState
  , zoomHdAccountCheckpoints
  , zoomHdAccountCurrent
  , matchHdAccountCheckpoints
    -- * Zoom variations that create on request
  , zoomOrCreateHdRoot
  , zoomOrCreateHdAccount
  , zoomOrCreateHdAddress
  , assumeHdRootExists
  , assumeHdAccountExists
    -- * General-utility functions
  , eskToHdRootId
  ) where

import           Universum

import           Control.Lens (at, (+~), _Wrapped)
import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import qualified Data.IxSet.Typed as IxSet (Indexable (..))
import           Data.SafeCopy (SafeCopy (..), base, deriveSafeCopy)

import           Test.QuickCheck (Arbitrary (..), oneof, vectorOf)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..))
import qualified Pos.Crypto as Core

import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet hiding (Indexable)
import           Cardano.Wallet.Kernel.Util (modifyAndGetOld, neHead)

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName { getWalletName :: Text }

-- | Account name
newtype AccountName = AccountName { getAccountName :: Text }

-- | Account index
newtype HdAccountIx = HdAccountIx { getHdAccountIx :: Word32 }
  deriving (Eq, Ord)

-- NOTE(adn) if we need to generate only @hardened@ account indexes, we
-- need to extend this arbitrary instance accordingly.
instance Arbitrary HdAccountIx where
    arbitrary = HdAccountIx <$> arbitrary

-- | Address index
newtype HdAddressIx = HdAddressIx { getHdAddressIx :: Word32 }
  deriving (Eq, Ord)

instance Arbitrary HdAddressIx where
    arbitrary = HdAddressIx <$> arbitrary

-- | Wallet assurance level
data AssuranceLevel =
    AssuranceLevelNormal
  | AssuranceLevelStrict

-- | Interpretation of 'AssuranceLevel'
--
-- This is adopted from the legacy wallet, which claims that these values
-- are taken from <https://cardanodocs.com/cardano/transaction-assurance/>
assuredBlockDepth :: AssuranceLevel -> Core.BlockCount
assuredBlockDepth AssuranceLevelNormal = 9
assuredBlockDepth AssuranceLevelStrict = 15

-- | Does this wallet have a spending password
data HasSpendingPassword =
    -- | No spending password set
    NoSpendingPassword

    -- | If there is a spending password, we record when it was last updated.
  | HasSpendingPassword (InDb Core.Timestamp)

deriveSafeCopy 1 'base ''WalletName
deriveSafeCopy 1 'base ''AccountName
deriveSafeCopy 1 'base ''HdAccountIx
deriveSafeCopy 1 'base ''HdAddressIx
deriveSafeCopy 1 'base ''AssuranceLevel
deriveSafeCopy 1 'base ''HasSpendingPassword

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- | Computes the 'HdRootId' from the given 'EncryptedSecretKey'. See the
-- comment in the definition of 'makePubKeyAddressBoot' on why this is
-- acceptable.
--
-- TODO: This may well disappear as part of [CBR-325].
eskToHdRootId :: Core.EncryptedSecretKey -> HdRootId
eskToHdRootId = HdRootId . InDb . Core.makePubKeyAddressBoot . Core.encToPublic


{-------------------------------------------------------------------------------
  HD wallets
-------------------------------------------------------------------------------}

-- | HD wallet root ID.
--
-- Conceptually, this is just an 'Address' in the form
-- of 'Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxxJgxdAYHrDeSCSbCxrvx', but is,
-- in a sense, a special breed as it's derived from the 'PublicKey' (derived
-- from some BIP-39 mnemonics, typically) and which does not depend from any
-- delegation scheme, as you cannot really pay into this 'Address'. This
-- ensures that, given an 'EncryptedSecretKey' we can derive its 'PublicKey'
-- and from that the 'Core.Address'.
-- On the \"other side\", given a RESTful 'WalletId' (which is ultimately
-- just a Text) it's possible to call 'decodeTextAddress' to grab a valid
-- 'Core.Address', and then transform this into a 'Kernel.WalletId' type
-- easily.
--
-- NOTE: Comparing 'HdRootId' is a potentially expensive computation, as it
-- implies comparing large addresses. Use with care.
--
-- TODO: It would be better not to have the address here, and just use an 'Int'
-- as a primary key. This however is a slightly larger refactoring we don't
-- currently have time for.
newtype HdRootId = HdRootId { getHdRootId :: InDb Core.Address }
  deriving (Eq, Ord)

instance Arbitrary HdRootId where
  arbitrary = do
      (_, esk) <- Core.safeDeterministicKeyGen <$> (BS.pack <$> vectorOf 12 arbitrary)
                                               <*> pure mempty
      pure (eskToHdRootId esk)

-- | HD wallet account ID
data HdAccountId = HdAccountId {
      _hdAccountIdParent :: HdRootId
    , _hdAccountIdIx     :: HdAccountIx
    }
  deriving (Eq)

-- | We make sure to compare the account index first to avoid doing an
-- unnecessary comparison of the root ID
instance Ord HdAccountId where
  compare a b =
       compare (_hdAccountIdIx     a) (_hdAccountIdIx     b)
    <> compare (_hdAccountIdParent a) (_hdAccountIdParent b)

instance Arbitrary HdAccountId where
  arbitrary = HdAccountId <$> arbitrary <*> arbitrary

-- | HD wallet address ID
data HdAddressId = HdAddressId {
      _hdAddressIdParent :: HdAccountId
    , _hdAddressIdIx     :: HdAddressIx
    }
  deriving (Eq)

-- | We make sure to compare the address index first to avoid doing an
-- unnecessary comparison of the account ID
instance Ord HdAddressId where
  compare a b =
       compare (_hdAddressIdIx     a) (_hdAddressIdIx     b)
    <> compare (_hdAddressIdParent a) (_hdAddressIdParent b)

instance Arbitrary HdAddressId where
  arbitrary = HdAddressId <$> arbitrary <*> arbitrary

-- | Root of a HD wallet
--
-- The wallet has sequentially assigned account indices and randomly assigned
-- address indices.
--
-- NOTE: We do not store the encrypted key of the wallet.
data HdRoot = HdRoot {
      -- | Wallet ID
      _hdRootId          :: HdRootId

      -- | Wallet name
    , _hdRootName        :: WalletName

      -- | Does this wallet have a spending password?
      --
      -- NOTE: We do not store the spending password itself, but merely record
      -- whether there is one. Updates to the spending password affect only the
      -- external key storage, not the wallet DB proper.
    , _hdRootHasPassword :: HasSpendingPassword

      -- | Assurance level
    , _hdRootAssurance   :: AssuranceLevel

      -- | When was this wallet created?
    , _hdRootCreatedAt   :: InDb Core.Timestamp
    }

-- | Account in a HD wallet
--
-- Key derivation is cheap
data HdAccount = HdAccount {
      -- | Account index
      _hdAccountId            :: HdAccountId

      -- | Account name
    , _hdAccountName          :: AccountName

      -- | Account state
      --
      -- When the account is up to date with the blockchain, the account state
      -- coincides with the state of a " wallet " as mandated by the formal
      -- spec.
    , _hdAccountState         :: HdAccountState

      -- | A local counter used to generate new 'AutoIncrementKey' for
      -- addresses.
    , _hdAccountAutoPkCounter :: AutoIncrementKey
    }

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | Address ID
      _hdAddressId      :: HdAddressId

      -- | The actual address
    , _hdAddressAddress :: InDb Core.Address
    }

{-------------------------------------------------------------------------------
  Account state
-------------------------------------------------------------------------------}

-- | Account state (essentially, how much historical data do we have?)
data HdAccountState =
      HdAccountStateUpToDate !HdAccountUpToDate
    | HdAccountStateWithinK  !HdAccountWithinK
    | HdAccountStateOutsideK !HdAccountOutsideK

-- | Account state for an account which has complete historical data
data HdAccountUpToDate = HdAccountUpToDate {
      _hdUpToDateCheckpoints :: !(NewestFirst NonEmpty Checkpoint)
    }

-- | Account state for an account which is lacking some historical checkpoints,
-- but is within k slots of the tip.
--
-- NOTE: If the wallet backend gets shut down during restoration, and later
-- restarted, it cannot be the case that the wallet is behind the full node,
-- since the full node /itself/ will also be behind the chain. The wallet can
-- /only/ be behind the full node if a wallet (that already exists on the chain)
-- gets added to a running full node.
data HdAccountWithinK = HdAccountWithinK {
      -- | Current checkpoints
      --
      -- During wallet restoration we always track the underlying node, but may
      -- lack historical checkpoints. We synchronously construct a partial
      -- checkpoint for the current tip, and then as we get new blocks from
      -- the BListener, we add new partial checkpoints.
      _hdWithinKCurrent    :: !(NewestFirst NonEmpty PartialCheckpoint)

      -- | Historical full checkpoints
      --
      -- Meanwhile, we asynchronously construct full checkpoints, starting
      -- from genesis. Once this gets to within k slots of the tip, we start
      -- keeping all of these.
    , _hdWithinKHistorical :: !(NewestFirst NonEmpty Checkpoint)
    }

-- | Account state for an account which is lacking historical checkpoints,
-- and hasn't reached the block that is within k slots of the tip yet.
data HdAccountOutsideK = HdAccountOutsideK {
      -- | Current checkpoint
      _hdOutsideKCurrent    :: !(NewestFirst NonEmpty PartialCheckpoint)

      -- | Historical full checkpoints
      --
      -- Since we haven't reached the block k away yet, we only need to
      -- keep this one checkpoint.
    , _hdOutsideKHistorical :: !Checkpoint
    }

makeLenses ''HdAccountUpToDate
makeLenses ''HdAccountWithinK
makeLenses ''HdAccountOutsideK

-- | Once we reached K slots from the tip, we should start collecting
-- checkpoints rather than just keeping the most recent.
reachedWithinK :: HdAccountOutsideK -> HdAccountWithinK
reachedWithinK HdAccountOutsideK{..} = HdAccountWithinK{
      _hdWithinKCurrent    = _hdOutsideKCurrent
    , _hdWithinKHistorical = NewestFirst $ _hdOutsideKHistorical :| []
    }

-- | Restoration is complete when we have all historical checkpoints
--
-- NOTE: The local block metadata in the partial checkpoints /already/
-- accumulates (the local block metadata in the next partial checkpoint includes
-- the local block metadata in the previous). Therefore we get the most recent
-- /full/ checkpoint, and use that as the basis for constructing full block
-- metadata for /all/ partial checkpoints.
finishRestoration :: HdAccountWithinK -> HdAccountUpToDate
finishRestoration HdAccountWithinK{..} = HdAccountUpToDate{
      _hdUpToDateCheckpoints =
           map (toFullCheckpoint mostRecent) _hdWithinKCurrent
        <> _hdWithinKHistorical
    }
  where
    NewestFirst (mostRecent :| _) = _hdWithinKHistorical

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''HdAccountId
makeLenses ''HdAddressId

makeLenses ''HdRoot
makeLenses ''HdAccount
makeLenses ''HdAddress

deriveSafeCopy 1 'base ''HdRootId
deriveSafeCopy 1 'base ''HdAccountId
deriveSafeCopy 1 'base ''HdAddressId

deriveSafeCopy 1 'base ''HdRoot
deriveSafeCopy 1 'base ''HdAccount
deriveSafeCopy 1 'base ''HdAddress

deriveSafeCopy 1 'base ''HdAccountState
deriveSafeCopy 1 'base ''HdAccountUpToDate
deriveSafeCopy 1 'base ''HdAccountWithinK
deriveSafeCopy 1 'base ''HdAccountOutsideK

instance SafeCopy (NewestFirst NonEmpty Checkpoint)  where
    getCopy = error "TODO: getCopy for (NewestFirst NonEmpty Checkpoint)"
    putCopy = error "TODO: putCopy for (NewestFirst NonEmpty Checkpoint)"

instance SafeCopy (NewestFirst NonEmpty PartialCheckpoint)  where
    getCopy = error "TODO: getCopy for (NewestFirst NonEmpty Checkpoint)"
    putCopy = error "TODO: putCopy for (NewestFirst NonEmpty Checkpoint)"

{-------------------------------------------------------------------------------
  Derived lenses
-------------------------------------------------------------------------------}

hdAccountRootId :: Lens' HdAccount HdRootId
hdAccountRootId = hdAccountId . hdAccountIdParent

hdAddressAccountId :: Lens' HdAddress HdAccountId
hdAddressAccountId = hdAddressId . hdAddressIdParent

hdAddressRootId :: Lens' HdAddress HdRootId
hdAddressRootId = hdAddressAccountId . hdAccountIdParent

hdAccountStateCurrent :: Lens' HdAccountState PartialCheckpoint
hdAccountStateCurrent f (HdAccountStateUpToDate st) =
    (\pcp -> HdAccountStateUpToDate (st & l .~ pcp)) <$> f (st ^. l)
  where
    l :: Lens' HdAccountUpToDate PartialCheckpoint
    l = hdUpToDateCheckpoints . _Wrapped . neHead . fromFullCheckpoint
hdAccountStateCurrent f (HdAccountStateWithinK st) =
    (\pcp -> HdAccountStateWithinK (st & l .~ pcp)) <$> f (st ^. l)
  where
    l :: Lens' HdAccountWithinK PartialCheckpoint
    l = hdWithinKCurrent . _Wrapped . neHead
hdAccountStateCurrent f (HdAccountStateOutsideK st) =
    (\pcp -> HdAccountStateOutsideK (st & l .~ pcp)) <$> f (st ^. l)
  where
    l :: Lens' HdAccountOutsideK PartialCheckpoint
    l = hdOutsideKCurrent . _Wrapped . neHead

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown root
data UnknownHdRoot =
    -- | Unknown root ID
    UnknownHdRoot HdRootId
    deriving Eq

instance Arbitrary UnknownHdRoot where
    arbitrary = oneof [ UnknownHdRoot <$> arbitrary
                      ]

-- | Unknown account
data UnknownHdAccount =
    -- | Unknown root ID
    UnknownHdAccountRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAccount HdAccountId
  deriving Eq

instance Arbitrary UnknownHdAccount where
    arbitrary = oneof [ UnknownHdAccountRoot <$> arbitrary
                      , UnknownHdAccount <$> arbitrary
                      ]

-- | Unknown address
data UnknownHdAddress =
    -- | Unknown root ID
    UnknownHdAddressRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAddressAccount HdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownHdAddress HdAddressId

    -- | Unknown address (implies it was not derived from the given Address)
  | UnknownHdCardanoAddress Core.Address

embedUnknownHdRoot :: UnknownHdRoot -> UnknownHdAccount
embedUnknownHdRoot = go
  where
    go (UnknownHdRoot rootId) = UnknownHdAccountRoot rootId

embedUnknownHdAccount :: UnknownHdAccount -> UnknownHdAddress
embedUnknownHdAccount = go
  where
    go (UnknownHdAccountRoot rootId) = UnknownHdAddressRoot rootId
    go (UnknownHdAccount accountId)  = UnknownHdAddressAccount accountId

deriveSafeCopy 1 'base ''UnknownHdRoot
deriveSafeCopy 1 'base ''UnknownHdAddress
deriveSafeCopy 1 'base ''UnknownHdAccount

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey HdRoot where
    type PrimKey HdRoot = HdRootId
    primKey = _hdRootId

instance HasPrimKey HdAccount where
    type PrimKey HdAccount = HdAccountId
    primKey = _hdAccountId

instance HasPrimKey (Indexed HdAddress) where
    type PrimKey (Indexed HdAddress) = HdAddressId
    primKey = _hdAddressId . _ixedIndexed

type SecondaryHdRootIxs           = '[]
type SecondaryHdAccountIxs        = '[HdRootId]
type SecondaryIndexedHdAddressIxs = '[AutoIncrementKey, HdRootId, HdAccountId, V1 Core.Address]

type instance IndicesOf HdRoot              = SecondaryHdRootIxs
type instance IndicesOf HdAccount           = SecondaryHdAccountIxs
type instance IndicesOf (Indexed HdAddress) = SecondaryIndexedHdAddressIxs

instance IxSet.Indexable (HdRootId ': SecondaryHdRootIxs)
                         (OrdByPrimKey HdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': SecondaryHdAccountIxs)
                         (OrdByPrimKey HdAccount) where
    indices = ixList
                (ixFun ((:[]) . view hdAccountRootId))

instance IxSet.Indexable (HdAddressId ': SecondaryIndexedHdAddressIxs)
                         (OrdByPrimKey (Indexed HdAddress)) where
    indices = ixList
                (ixFun ((:[]) . view ixedIndex))
                (ixFun ((:[]) . view (ixedIndexed . hdAddressRootId)))
                (ixFun ((:[]) . view (ixedIndexed . hdAddressAccountId)))
                (ixFun ((:[]) . V1 . view (ixedIndexed . hdAddressAddress . fromDb)))

{-------------------------------------------------------------------------------
  Top-level HD wallet structure
-------------------------------------------------------------------------------}

-- | All wallets, accounts and addresses in the HD wallets
--
-- We use a flat "relational" structure rather than nested maps so that we can
-- go from address to wallet just as easily as the other way around.
data HdWallets = HdWallets {
    _hdWalletsRoots     :: IxSet HdRoot
  , _hdWalletsAccounts  :: IxSet HdAccount
  , _hdWalletsAddresses :: IxSet (Indexed HdAddress)
  }

deriveSafeCopy 1 'base ''HdWallets
makeLenses ''HdWallets

initHdWallets :: HdWallets
initHdWallets = HdWallets IxSet.empty IxSet.empty IxSet.empty

{-------------------------------------------------------------------------------
  Zoom to existing parts of a HD wallet
-------------------------------------------------------------------------------}

zoomHdRootId :: forall f e a. CanZoom f
             => (UnknownHdRoot -> e)
             -> HdRootId
             -> f HdRoot e a -> f HdWallets e a
zoomHdRootId embedErr rootId =
    zoomDef err (hdWalletsRoots . at rootId)
  where
    err :: f HdWallets e a
    err = missing $ embedErr (UnknownHdRoot rootId)

zoomHdAccountId :: forall f e a. CanZoom f
                => (UnknownHdAccount -> e)
                -> HdAccountId
                -> f HdAccount e a -> f HdWallets e a
zoomHdAccountId embedErr accId =
    zoomDef err (hdWalletsAccounts . at accId)
  where
    err :: f HdWallets e a
    err = zoomHdRootId embedErr' (accId ^. hdAccountIdParent) $
            missing $ embedErr (UnknownHdAccount accId)

    embedErr' :: UnknownHdRoot -> e
    embedErr' = embedErr . embedUnknownHdRoot

zoomHdAddressId :: forall f e a. CanZoom f
                => (UnknownHdAddress -> e)
                -> HdAddressId
                -> f HdAddress e a -> f HdWallets e a
zoomHdAddressId embedErr addrId =
    zoomDef err (hdWalletsAddresses . at addrId) . zoom ixedIndexed
  where
    err :: f HdWallets e a
    err = zoomHdAccountId embedErr' (addrId ^. hdAddressIdParent) $
            missing $ embedErr (UnknownHdAddress addrId)

    embedErr' :: UnknownHdAccount -> e
    embedErr' = embedErr . embedUnknownHdAccount

-- | Zoom to the specified Cardano address
--
-- This is defined on queries only for now. In principle we could define this
-- more generally, but that would require somehow taking advantage of the  fact
-- that there cannot be more than one 'HdAddress' with a given 'Core.Address'.
zoomHdCardanoAddress :: forall e a.
                        (UnknownHdAddress -> e)
                     -> Core.Address
                     -> Query' HdAddress e a -> Query' HdWallets e a
zoomHdCardanoAddress embedErr addr =
    localQuery findAddress
  where
    findAddress :: Query' HdWallets e HdAddress
    findAddress = do
        addresses <- view hdWalletsAddresses
        maybe err return $ (fmap _ixedIndexed $ getOne $ getEQ (V1 addr) addresses)

    err :: Query' HdWallets e x
    err = missing $ embedErr (UnknownHdCardanoAddress addr)

-- | Pattern match on the state of the account
matchHdAccountState :: CanZoom f
                    => f HdAccountUpToDate e a
                    -> f HdAccountWithinK  e a
                    -> f HdAccountOutsideK e a
                    -> f HdAccount         e a
matchHdAccountState updUpToDate updWithinK updOutsideK = withZoom $ \acc zoomTo ->
    case acc ^. hdAccountState of
      HdAccountStateUpToDate st ->
        zoomTo st (\st' -> acc & hdAccountState .~ HdAccountStateUpToDate st') updUpToDate
      HdAccountStateWithinK  st ->
        zoomTo st (\st' -> acc & hdAccountState .~ HdAccountStateWithinK  st') updWithinK
      HdAccountStateOutsideK st ->
        zoomTo st (\st' -> acc & hdAccountState .~ HdAccountStateOutsideK st') updOutsideK

-- | Zoom to the current checkpoints of the wallet
zoomHdAccountCheckpoints :: CanZoom f
                         => (   forall c. IsCheckpoint c
                             => f (NewestFirst NonEmpty c) e a )
                         -> f HdAccount e a
zoomHdAccountCheckpoints upd =
    matchHdAccountState
      (zoom hdUpToDateCheckpoints upd)
      (zoom hdWithinKCurrent      upd)
      (zoom hdOutsideKCurrent     upd)

-- | Zoom to the most recent checkpoint
zoomHdAccountCurrent :: CanZoom f
                     => (forall c. IsCheckpoint c => f c e a)
                     -> f HdAccount e a
zoomHdAccountCurrent upd =
    zoomHdAccountCheckpoints $
      withZoom $ \cps zoomTo -> do
        let l :: Lens' (NewestFirst NonEmpty c) c
            l = _Wrapped . neHead
        zoomTo (cps ^. l) (\cp' -> cps & l .~ cp') upd

-- | Variant of 'zoomHdAccountCheckpoints' that distinguishes between
-- full checkpoints (wallet is up to date) and partial checkpoints
-- (wallet is still recovering historical data)
matchHdAccountCheckpoints :: CanZoom f
                          => f (NewestFirst NonEmpty Checkpoint)        e a
                          -> f (NewestFirst NonEmpty PartialCheckpoint) e a
                          -> f HdAccount e a
matchHdAccountCheckpoints updFull updPartial =
    matchHdAccountState
      (zoom hdUpToDateCheckpoints updFull)
      (zoom hdWithinKCurrent      updPartial)
      (zoom hdOutsideKCurrent     updPartial)

{-------------------------------------------------------------------------------
  Zoom to parts of the wallet, creating them if they don't exist
-------------------------------------------------------------------------------}

-- | Variation on 'zoomHdRootId' that creates the 'HdRoot' if it doesn't exist
--
-- Precondition: @newRoot ^. hdRootId == rootId@
zoomOrCreateHdRoot :: HdRoot
                   -> HdRootId
                   -> Update' HdRoot    e a
                   -> Update' HdWallets e a
zoomOrCreateHdRoot newRoot rootId upd =
    zoomCreate (return newRoot) (hdWalletsRoots . at rootId) $ upd

-- | Variation on 'zoomHdAccountId' that creates the 'HdAccount' if it doesn't exist
--
-- Precondition: @newAccount ^. hdAccountId == accountId@
zoomOrCreateHdAccount :: (HdRootId -> Update' HdWallets e ())
                      -> HdAccount
                      -> HdAccountId
                      -> Update' HdAccount e a
                      -> Update' HdWallets e a
zoomOrCreateHdAccount checkRootExists newAccount accId upd = do
    checkRootExists $ accId ^. hdAccountIdParent
    zoomCreate (return newAccount) (hdWalletsAccounts . at accId) $ upd

-- | Variation on 'zoomHdAddressId' that creates the 'HdAddress' if it doesn't exist
--
-- Precondition: @newAddress ^. hdAddressId == AddressId@
zoomOrCreateHdAddress :: (HdAccountId -> Update' HdWallets e ())
                      -> HdAddress
                      -> HdAddressId
                      -> Update' HdAddress e a
                      -> Update' HdWallets e a
zoomOrCreateHdAddress checkAccountExists newAddress addrId upd = do
    checkAccountExists accId
    zoomCreate createAddress
               (hdWalletsAddresses . at addrId) . zoom ixedIndexed $ upd
    where
        accId :: HdAccountId
        accId = addrId ^. hdAddressIdParent

        createAddress :: Update' HdWallets e (Indexed HdAddress)
        createAddress = do
            let err = "zoomOrCreateHdAddress: we checked that the account existed "
                   <> "before calling this function, but the DB lookup failed nonetheless."
            acc <- zoomHdAccountId (error err) accId $ do
                      modifyAndGetOld (hdAccountAutoPkCounter +~ 1)
            return $ Indexed (acc ^. hdAccountAutoPkCounter) newAddress

-- | Assume that the given HdRoot exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAccount'
assumeHdRootExists :: HdRootId -> Update' HdWallets e ()
assumeHdRootExists _id = return ()

-- | Assume that the given HdAccount exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAddress'
assumeHdAccountExists :: HdAccountId -> Update' HdWallets e ()
assumeHdAccountExists _id = return ()

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable WalletName where
    build (WalletName wName) = bprint build wName

instance Buildable AccountName where
    build (AccountName txt) = bprint build txt

instance Buildable AssuranceLevel where
    build AssuranceLevelNormal = "normal"
    build AssuranceLevelStrict = "strict"

instance Buildable HasSpendingPassword where
    build NoSpendingPassword = "no"
    build (HasSpendingPassword (InDb lastUpdate)) =
        bprint ("updated " % build) lastUpdate

instance Buildable HdRoot where
    build HdRoot{..} = bprint
      ( "HdRoot "
      % "{ id:          " % build
      % ", name:        " % build
      % ", hasPassword: " % build
      % ", assurance:   " % build
      % ", createdAt:   " % build
      % "}"
      )
      _hdRootId
      _hdRootName
      _hdRootHasPassword
      _hdRootAssurance
      (_fromDb _hdRootCreatedAt)

instance Buildable HdAccount where
    build HdAccount{..} = bprint
      ( "HdAccount "
      % "{ id            " % build
      % ", name          " % build
      % ", state         " % build
      % ", autoPkCounter " % build
      % "}"
      )
      _hdAccountId
      _hdAccountName
      _hdAccountState
      _hdAccountAutoPkCounter

instance Buildable HdAccountState where
    build (HdAccountStateUpToDate _cps) = "HdAccountStateUpToDate <checkpoints>"
    build (HdAccountStateWithinK  _cps) = "HdAccountStateWithinK <checkpoints>"
    build (HdAccountStateOutsideK _cps) = "HdAccountStateOutsideK <checkpoints>"

instance Buildable HdAddress where
    build HdAddress{..} = bprint
      ( "HdAddress "
      % "{ id      " % build
      % ", address " % build
      % "}"
      )
      _hdAddressId
      (_fromDb _hdAddressAddress)

instance Buildable HdRootId where
    build (HdRootId addr) = bprint ("HdRootId " % build) (_fromDb addr)

instance Buildable HdAccountId where
    build HdAccountId{..} = bprint
      ( "HdAccountId "
      % "{ parent " % build
      % ", ix     " % build
      % "}"
      )
      _hdAccountIdParent
      _hdAccountIdIx

instance Buildable HdAddressId where
    build HdAddressId{..} = bprint
      ( "HdAddressId "
      % " parent " % build
      % " ix     " % build
      % "}"
      )
      _hdAddressIdParent
      _hdAddressIdIx

instance Buildable HdAccountIx where
    build (HdAccountIx ix) = bprint ("HdAccountIx " % build) ix

instance Buildable HdAddressIx where
    build (HdAddressIx ix) = bprint ("HdAddressIx " % build) ix

instance Buildable UnknownHdRoot where
    build (UnknownHdRoot rootId) = bprint ("UnknownHdRoot " % build) rootId

instance Buildable UnknownHdAccount where
    build (UnknownHdAccountRoot rootId) =
      bprint ("UnknownHdAccountRoot " % build) rootId
    build (UnknownHdAccount accountId) =
      bprint ("UnknownHdAccount accountId " % build) accountId
