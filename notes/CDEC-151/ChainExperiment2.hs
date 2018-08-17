{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ChainExperiment2 where

-- import Data.Word
import Data.Ord (Down (..))
import Data.List (foldl', intersect, sortOn, tails)
import Data.Hashable
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Max (..))

-- import Control.Applicative
import Control.Monad (replicateM)

import Test.QuickCheck


--
-- Simple blockchain data type.
--

type Chain = [Block]  -- most recent block at the front

data Block = Block {
       blockId      :: BlockId,  -- ^ hash of other fields
       prevBlockId  :: BlockId,  -- ^ 'blockId' of the previous block
       blockSlot    :: Slot,
       blockPayload :: Payload
     }
  deriving (Show, Eq)

type BlockId = Int
type Slot    = Word
type Payload = String


hashBlock :: Block -> BlockId
hashBlock Block{prevBlockId, blockSlot, blockPayload} =
    hash (prevBlockId, blockSlot, blockPayload)

--
-- What it means for a chain to be valid
--

validChain :: Chain -> Bool
validChain []     = True
validChain (b:bs) = validChainExtension b bs && validChain bs

validChainExtension :: Block -> Chain -> Bool
validChainExtension b _
  | blockId b /= hashBlock b = False

validChainExtension b []     = prevBlockId b == 0
validChainExtension b (b':_) = prevBlockId b == blockId b'
                            && blockSlot b > blockSlot b'

--
-- And useful later: chain fragments
-- 

-- | Like 'Chain but does not have to chain onto the genesis block. Its final
-- back pointer can be anything at all.
--
type ChainFragment = [Block]

validChainFragment :: ChainFragment -> Bool
validChainFragment []     = True
validChainFragment (b:bs) = validChainFragmentExtension b bs
                         && validChainFragment bs

validChainFragmentExtension :: Block -> Chain -> Bool
validChainFragmentExtension b _
  | blockId b /= hashBlock b = False

validChainFragmentExtension _ []     = True -- any prevBlockId is ok
validChainFragmentExtension b (b':_) = prevBlockId b == blockId b'
                                    && blockSlot b > blockSlot b'

--
-- Generating valid chains
--

mkBlock :: BlockId -> Slot -> Payload -> Block
mkBlock blockid' slot payload = block
  where
    block   = Block blockid blockid' slot payload
    blockid = hashBlock block

genBlock :: BlockId -> Slot -> Gen Block
genBlock blockid slot = do
    payload <- vectorOf 4 (choose ('A', 'Z'))
    return (mkBlock blockid slot payload)

genNBlocks :: Int -> BlockId -> Slot -> Gen [Block]
genNBlocks 0 _       _    = return []
genNBlocks 1 blockid slot = (:[]) <$> genBlock blockid slot
genNBlocks n blockid slot = do
    c@(b':_) <- genNBlocks (n-1) blockid slot
    b        <- genBlock (blockId b') (blockSlot b' + 1)
    return (b:c)

genChain :: Int -> Gen Chain
genChain n = genNBlocks n 0 1

newtype TestChain = TestChain Chain
  deriving Show

instance Arbitrary TestChain where
  arbitrary = do
    Positive n <- arbitrary
    TestChain <$> genChain n

prop_TestChain :: TestChain -> Bool
prop_TestChain (TestChain chain) = validChain chain

--
-- The operation on the abstract type
--

data ChainUpdate = AddBlock   Block
                 | SwitchFork Int     -- rollback by n
                              [Block] -- add more blocks
                                      -- TODO (@coot): I think we should also
                                      -- say the tip from which switch the fork.
                                      -- It may happend that the client tip will
                                      -- change (by listening to another
                                      -- producer).
  deriving Show

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock     b)  c = b:c
applyChainUpdate (SwitchFork n bs) c = bs ++ drop n c

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

validChainUpdate :: ChainUpdate -> Chain -> Bool
validChainUpdate cu c = validChainUpdate' cu
                     && validChain (applyChainUpdate cu c)

validChainUpdate' :: ChainUpdate -> Bool
validChainUpdate' (AddBlock    _b)  = True
validChainUpdate' (SwitchFork n bs) = n >= 0 && n <= k && length bs == n + 1

k :: Int
k = 5 -- maximum fork length in these tests

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId []    = 0
chainHeadBlockId (b:_) = blockId b

chainHeadSlot :: Chain -> Slot
chainHeadSlot []    = 0
chainHeadSlot (b:_) = blockSlot b

--
-- Generating valid chain updates
--

genChainUpdate :: Chain -> Gen ChainUpdate
genChainUpdate chain = do
    let maxRollback = length (take k chain)
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock     <$> genBlock (chainHeadBlockId chain)
                                     (chainHeadSlot chain + 1)
      else SwitchFork n <$> let chain' = drop n chain in
                            genNBlocks (n+1) (chainHeadBlockId chain')
                                             (chainHeadSlot chain' + 1)

genChainUpdates :: Chain -> Int -> Gen [ChainUpdate]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)

data TestChainAndUpdates = TestChainAndUpdates Chain [ChainUpdate]
  deriving Show

instance Arbitrary TestChainAndUpdates where
  arbitrary = do
    (Positive n, Positive m) <- arbitrary
    chain   <- genChain n
    updates <- genChainUpdates chain m
    return (TestChainAndUpdates chain updates)

prop_TestChainAndUpdates :: TestChainAndUpdates -> Bool
prop_TestChainAndUpdates (TestChainAndUpdates chain updates) =
    all validChain chains
 && all (uncurry validChainUpdate) (zip updates chains)
  where
    chains = scanl (flip applyChainUpdate) chain updates

--
-- Data types for a plausibly-realisic representation of a blockchain.
--

-- | Represent a chain simply as a volatile chain fragment.
--
data ChainState = ChainState {
       chainVolatile :: Volatile
     }
  deriving (Eq, Show)

-- | Representation of a chain fragment as a graph with backwards and forward
-- pointers.
--
data Volatile = Volatile
                  (Map BlockId (Block, Maybe BlockId))
                  (Maybe BlockId) -- ^ current tip, or empty
  deriving (Eq, Show)

newtype TestVolatile = TestVolatile { runTestVolatile :: Volatile }
    deriving (Show)

instance Arbitrary TestVolatile where
    shrink (TestVolatile (Volatile blocks _)) =
      case [b | (b, Nothing) <- Map.elems blocks ] of
        []     -> []
        [tip]  ->
          let as = fromTip tip
              -- split the @as@ tine into two unequal halfs ;)
          in case splitAt (length as `div` 2) as of
              ([], [])                 -> []
              ([], tine@((_, (tip, _)) : _))  ->
                [TestVolatile $ Volatile (Map.fromList tine) (Just $ blockId tip)]
              (tine@((_, (tip, _)) : _), [])  ->
                [TestVolatile $ Volatile (Map.fromList tine) (Just $ blockId tip)]
              (tine@((_, (tip, _)) : _), tine'@((_, (tip', _)) : _))  ->
                [ TestVolatile $ Volatile (Map.fromList tine)  (Just $ blockId tip)
                , TestVolatile $ Volatile (Map.fromList tine') (Just $ blockId tip')
                ]
        -- return a list of all tines
        tips  -> map (\b -> TestVolatile $ Volatile (Map.fromList (fromTip b)) (Just $ blockId b)) tips

      where
      fromTip :: Block -> [(BlockId, (Block, Maybe BlockId))]
      fromTip b = map (\a -> (blockId (fst a), a)) $ chainBackwardsFrom' blocks (blockId b)

    arbitrary = sized $ \maxTine ->
        sized $ \maxForks -> do
            bot <- genBlock 0 0
            blocks <- genVolatile maxTine maxForks [(bot, Nothing)]
            let tipId = case blocks of
                    []         -> Nothing
                    (b, _) : _ -> Just (blockId b)
            return $ TestVolatile $ Volatile (Map.fromList $ map (\(b, t) -> (blockId b, (b, t))) blocks) tipId
        where

        genVolatile :: Int -> Int -> [(Block, Maybe BlockId)] -> Gen [(Block, Maybe BlockId)]
        genVolatile 0      _        bs = return bs
        genVolatile _      0        bs = return bs
        genVolatile maxLen maxForks v =
            case v of
                []       -> error "genVolatile: unexpected empty volatile fork"
                tip : bs -> do
                    fork <- genFork maxLen maxForks (fst tip)
                    case fork of
                        []       -> return (tip : bs)
                        [] : _   -> return (tip : bs)
                        tine : _ -> genVolatile (max 0 (maxLen - length tine)) maxForks
                            $ (concat fork) ++ ((fst tip, Just $ blockId $ fst $ last tine) : bs)

        genFork
            :: Int  -- ^ length of a longest tine
            -> Int  -- ^ number of forks
            -> Block
            -> Gen [[(Block, Maybe BlockId)]]
        genFork 0      _        _     = return []
        genFork maxLen maxForks block = do
            tineLen   <- choose (0, maxLen)
            tineForks <- choose (1, maxForks)
            
            forks <- replicateM tineForks $
                genTine tineLen (blockId block) (blockSlot block + 1)

            return $ sortOn (Down . length) forks

        genTine :: Int -- ^ maximal length of a tine
                -> BlockId
                -> Slot
                -> Gen [(Block, Maybe BlockId)]
        genTine 0      _   _    = return []
        genTine tineLen bid slot = do
            bs <- genNBlocks tineLen bid slot
            return $ zip bs (Nothing : map (Just . blockId) bs)

--
-- The data invariants
--

invChainState   :: ChainState -> Bool
invVolatile     :: Volatile   -> Bool

invChainState (ChainState v) =
    invVolatile  v

invVolatile (Volatile blocks Nothing) =
    -- The whole thing can be empty, with no tip.
    Map.null blocks

invVolatile (Volatile blocks (Just tip)) =
    -- But if it's not empty, then:
    and [
        -- The tip is in the map, and is marked as such
        case Map.lookup tip blocks of
          Just (_, Nothing) -> True; _ -> False

        -- There might be many tines in the volatile fork, and the tip must be
        -- end one of the tines.
      , elem tip [ blockId b | (b, Nothing) <- Map.elems blocks ]

        -- Tip must belong to the longest tine.
      , let tipTineLen = length $ chainBackwardsFrom blocks tip
        in tipTineLen >=
            getMax (foldMap Max
                      [ length (chainBackwardsFrom blocks (blockId tip'))
                      | (tip', Nothing) <- Map.elems blocks ])

        -- All blocks have the right key.
      , and [ b == b' | (b, (Block{blockId = b'}, _)) <- Map.toList blocks ]

        -- There is only one dangling back pointer.
      , length [ () | (b, _) <- Map.elems blocks
                    , prevBlockId b `Map.notMember` blocks ] == 1

        -- Back pointers have to be consistent with the forward pointer:
        -- following a back pointer gets a block that points forward to the same
        --
        -- NOTE: this property will not hold if there is a fork inside `blocks`
        --
        --      *
        --      |
        -- b *  * <- backward from b and then forward
        --    \ | 
        --      *
        --      |
      {--
        - , and [ case Map.lookup (prevBlockId b) blocks of
        -           Nothing                               -> True
        -           Just (_, Just bid) | bid == blockId b -> True
        -           _                                     -> False
        -       | (b, _) <- Map.elems blocks ]
        --}

        -- Forward pointers have to be consistent with the back pointer:
        -- following a forward pointer gets a block that points back to the
        -- same.
      , and [ case Map.lookup bid' blocks of
                Just (b',_) | prevBlockId b' == blockId b -> True
                _                                         -> False
            | (b, Just bid') <- Map.elems blocks ]

        -- The chain arising must form a valid chain fragment.
      , validChainFragment (chainBackwardsFrom blocks tip)

      ]

chainBackwardsFrom :: Map BlockId (Block, Maybe BlockId)
                   -> BlockId
                   -> [Block] -- ^ newest first
chainBackwardsFrom blocks bid =
    case Map.lookup bid blocks of
      Nothing    -> []
      Just (b,_) -> b : chainBackwardsFrom blocks (prevBlockId b)

-- |
-- @'chainBackwardsFrom'@ returns a list of blocks ordered from newest to
-- oldest.
invChainBackwardFrom
    :: Map BlockId (Block, Maybe BlockId)
    -> Bool
invChainBackwardFrom blocks = 
    all (\bid -> go (chainBackwardsFrom blocks bid)) (Map.keys blocks)
    where
    go :: [Block] -> Bool
    go []  = True
    go [_] = True
    go (x : y : ys) = prevBlockId x == blockId y && go (y : ys)

chainBackwardsFromTo
    :: Map BlockId (Block, Maybe BlockId)
    -> BlockId
    -- ^ from
    -> BlockId
    -- ^ to
    -> Maybe [Block]
    -- ^ newest first, it is guaranteed that the list ends on the block after
    -- @toBid@ block
chainBackwardsFromTo blocks fromBid toBid =
    case Map.lookup fromBid blocks of
        Nothing -> Nothing
        Just (b, _)
            | blockId b == toBid
                -> Just []
            | otherwise
                -> fmap (b :) $ chainBackwardsFromTo blocks (prevBlockId b) toBid

chainBackwardsFrom' :: Map BlockId (Block, Maybe BlockId)
                    -> BlockId
                    -> [(Block, Maybe BlockId)] -- ^ newest first
chainBackwardsFrom' blocks bid =
    case Map.lookup bid blocks of
      Nothing      -> []
      Just e@(b,_) -> e : chainBackwardsFrom' blocks (prevBlockId b)


chainForwardFrom :: Map BlockId (Block, Maybe BlockId)
                 -> BlockId
                 -> [Block] -- ^ oldest first
chainForwardFrom blocks blockId = go blockId []
    where
    go :: BlockId -> [Block] -> [Block]
    go bid !acu = case Map.lookup bid blocks of
        Nothing                 -> []
        Just (block, Nothing)   -> [block]
        Just (block, Just bid') -> go bid' (block : acu)

-- |
-- @'chainForwardFrom'@ returns a list of blocks ordered from  oldest to newest.
invChainForwardForm
    :: Map BlockId (Block, Maybe BlockId)
    -> Bool
invChainForwardForm blocks = all (\blockId -> go $ chainForwardFrom blocks blockId) (Map.keys blocks)
    where
    go []  = True
    go [_] = True
    go (x : y : ys) = blockId x == prevBlockId y && go (y : ys)

--
-- The abstraction function
--

absChainState :: ChainState -> Chain
absVolatile   :: Volatile   -> ChainFragment

absVolatile  (Volatile _      Nothing)    = []
absVolatile  (Volatile blocks (Just tip)) = chainBackwardsFrom blocks tip

absChainState (ChainState v) = absVolatile v

--
-- Step 1: empty chains
--

emptyChainState :: ChainState
emptyChainState = ChainState emptyVolatile

emptyVolatile :: Volatile
emptyVolatile  = Volatile Map.empty Nothing

-- the empty chain value should
-- 1. satisfy the invariant
-- 2. be equivalent to the empty chain abstract value [].
--
prop_emptyChainState :: Bool
prop_emptyChainState = invChainState emptyChainState
                    && absChainState emptyChainState == []

--
-- Step 2: adding single blocks
--

addBlock :: Block -> ChainState -> ChainState
addBlock b (ChainState v) = ChainState (addBlockVolatile b v)

addBlockVolatile :: Block -> Volatile -> Volatile
addBlockVolatile b (Volatile _ Nothing) =
    Volatile (Map.singleton (blockId b) (b, Nothing)) (Just (blockId b))

addBlockVolatile b' (Volatile blocks (Just tip))
  | prevBlockId b' == tip = Volatile blocks' (Just tip')
  | otherwise             = error "addBlockVolatile: wrong back pointer"
  where
    tip'    = blockId b'
    blocks' = Map.insert tip' (b', Nothing)
            . Map.adjust (\(b, _) -> (b, Just tip')) tip
            $ blocks

-- | For building a chain from empty using the 'addBlock', at each step
-- the invariant holds, and the concrete and abstract values are equivalent
--
prop_addBlock :: TestChain -> Bool
prop_addBlock (TestChain chain) =
    all invChainState steps
 && and [ absChainState c == c'
        | (c, c') <- zip (reverse steps) (tails chain) ]
  where
    steps = scanl (flip addBlock) emptyChainState (reverse chain)


--
-- Step 3: switching forks
--

switchFork :: Int -> [Block] -> ChainState -> ChainState
switchFork rollback newblocks (ChainState v) =
    ChainState (switchForkVolatile rollback newblocks v)

switchForkVolatile :: Int -> [Block] -> Volatile -> Volatile
switchForkVolatile _rollback _newblocks (Volatile _ Nothing) =
    error "switchForkVolatile: precondition violation"

-- TODO: perhaps we shouldn't remove blocks too eagerly or maybe we should have
-- a cache of blocks at some level so we don't need to redownload in case of two
-- competing tines.
switchForkVolatile rollback newblocks (Volatile blocks (Just tip)) =
    Volatile blocks' (Just tip')
  where
    tip'    = blockId (head newblocks)
    blocks' = fixLink . addBlocks forwards . delBlocks backwards $ blocks

    backwards :: [Block]
    backwards = take rollback (chainBackwardsFrom blocks tip)

    forwards :: [(Block, Maybe BlockId)]
    forwards  = zip newblocks (Nothing : map (Just . blockId) newblocks)

    addBlocks = flip (foldl' (\bs (b,fp) -> Map.insert (blockId b) (b,fp) bs))
    delBlocks = flip (foldl' (\bs  b     -> Map.delete (blockId b)        bs))
    fixLink   = Map.adjust (\(b,_) -> (b, Just rollforwardFrom)) rollbackTo
      where
        rollbackTo      = prevBlockId (last backwards)
        rollforwardFrom = blockId (last newblocks)

applyChainStateUpdate :: ChainUpdate -> ChainState -> ChainState
applyChainStateUpdate (AddBlock     b)  = addBlock b
applyChainStateUpdate (SwitchFork n bs) = switchFork n bs

-- | This is now the simulation property covering both the add block and
-- switch fork operations.
--
prop_switchFork :: TestChainAndUpdates -> Bool
prop_switchFork (TestChainAndUpdates chain updates) =
    all invChainState chains'
 && all (\(c, c') -> absChainState c' == c) (zip chains chains')
  where
    c0     = foldr addBlock emptyChainState chain

    chains' = scanl (flip applyChainStateUpdate) c0 updates
    chains  = scanl (flip applyChainUpdate)   chain updates

--
-- Read pointer operations
--

-- A 'ChainState' plus an associated set of readers/consumers of the chain.

data ChainProducerState = ChainProducerState {
       chainState   :: ChainState,
       chainReaders :: ReaderStates
     }

-- | Readers are represented here as a relation.
--
type ReaderStates = [ReaderState]

-- | A point on the chain is identified by the 'Slot' number and its 'BlockId'.
-- The 'Slot' tells us where to look and the 'BlockId' either simply serves as
-- a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
type Point        = (Slot, BlockId)
type ReaderId     = Int
data ReaderState  = ReaderState {
       -- | Where the chain of the consumer and producer intersect. If the
       -- consumer is on the chain then this is the same as the 'readerHead',
       -- but if the consumer 'readerHead' is off the chain then this is the
       -- point the consumer will need to rollback to.
       readerIntersection :: Point,

       -- | Where the chain consumer was last reading from (typically the
       -- head of the consumer's chain). If this is on the producer chain
       -- then it is equal to the 'readerIntersection'.
       readerHead         :: Point,

       -- | A unique tag per reader, to distinguish different readers.
       readerId           :: ReaderId
     }
  deriving (Eq, Show)

blockPoint :: Block -> Point
blockPoint b = (blockSlot b, blockId b)

invChainProducerState :: ChainProducerState -> Bool
invChainProducerState (ChainProducerState cs rs) =
    invChainState cs
 && invReaderStates cs rs

invReaderStates :: ChainState -> ReaderStates  -> Bool
invReaderStates cs rs =
    and [
        -- All the reader intersection points must be on the chain
        and [ pointOnChain cs readerIntersection
            | ReaderState{readerIntersection} <- rs ]

        -- All rollback pointer states start from blocks off the chain,
        -- and rollback must go backwards in slot number
      , and [ not (pointOnChain cs readerHead) &&
              fst readerIntersection < fst readerHead
            | ReaderState{readerIntersection, readerHead} <- rs
            , readerIntersection /= readerHead ]

      ]

pointOnChain :: ChainState -> Point -> Bool
pointOnChain (ChainState (Volatile blocks _)) (slot, bid) =
    case Map.lookup bid blocks of
      Just (block, _) -> blockSlot block == slot
      Nothing         -> False


{-
Hmm, perhaps this version does too much, lets simplify

initialiseReadPointer :: [Point]
                      -> ChainState
                      -> Maybe (ChainState, ReadPointer)
initialiseReadPointer checkpoints (ChainState v rs) = do
    (c, c') <- findIntersectionRange checkpoints
    let rs' = (c, readPtr, ) : rs
    return (ChainState v rs')
  where
    readPtr = freshReaderId rs

    findIntersectionRange cs =
      find (checkpointOnChain . fst)
           (zip cs (head cs ++ cs))

-}

initialiseReader :: Point
                      -> Point
                      -> ChainProducerState
                      -> Maybe (ChainProducerState, ReaderId)
initialiseReader pointReader pointIntersection (ChainProducerState cs rs)
    | not (pointOnChain cs pointIntersection)
    = Nothing

    | otherwise
    = Just (ChainProducerState cs (r:rs), readerId r)
  where
    r = ReaderState {
          readerIntersection = pointIntersection,
          readerHead         = pointReader,
          readerId           = freshReaderId rs
        }

freshReaderId :: ReaderStates -> ReaderId
freshReaderId rs = 1 + maximum [ readerId | ReaderState{readerId} <- rs ]

lookupReader :: ReaderStates -> ReaderId -> Maybe ReaderState
lookupReader rs rid = go rs
    where
    go [] = Nothing
    go (rs@ReaderState{readerId}:rs') | readerId == rid = Just rs
                                      | otherwise       = go rs'

readerInstructions
    :: ChainProducerState
    -> ReaderId
    -> Maybe [ConsumeChain Block]
readerInstructions (ChainProducerState (ChainState (Volatile blocks _)) crs) rid = do
    ReaderState{readerIntersection, readerHead} <- lookupReader crs rid
    let ccs = map RollForward $ chainForwardFrom blocks (snd readerHead)
    if readerIntersection == readerHead
        then Just ccs
        else do
            bs <- chainBackwardsFromTo blocks (snd readerHead) (snd readerIntersection)
            Just $ map (RollBackward . blockPoint) bs ++ ccs

-- |
-- Rollback pointers if we are switching to another tine (which will remove
-- blocks from volatile fork).
-- This function must be run before `switchForkVolatile` otherwise we will loose
-- pointers.
--
-- Assumptions:
--  * rollback should be shallower than the length of the longest tine,
--    otherewise pointers might get out of range
normalizeChainProducerState
    :: ChainUpdate
    -> ChainProducerState
    -> ChainProducerState
normalizeChainProducerState (AddBlock _) cps = cps
normalizeChainProducerState _ (ChainProducerState {chainState=ChainState (Volatile _ Nothing)})
    = error "normalizeChainState: precondition validation"
normalizeChainProducerState
    (SwitchFork n _)
    (cps@ChainProducerState
        { chainState   = cs@(ChainState (Volatile blocks (Just tip)))
        , chainReaders = rs
        })
    = case take (n + 1) $ chainBackwardsFrom blocks tip of
        [] -> cps
        bs ->
            let nextTip  = last bs
                newPoint = (blockSlot nextTip, blockId nextTip)
            in ChainProducerState
                { chainState   = cs
                , chainReaders = foldl' (updateReaderStates newPoint) rs (init bs)
                }
    where
    updatePointer
        :: Block -- ^ block which will be rolled back
        -> Point -- ^ new point
        -> Point -- ^ current point
        -> Point -- ^ updated point
    updatePointer b n o =
        if (blockSlot b, blockId b) == o
            then n
            else o

    updateReaderStates :: Point -> [ReaderState] -> Block -> [ReaderState]
    updateReaderStates p rs b = map (updateReaderState p b) rs

    updateReaderState :: Point -> Block -> ReaderState -> ReaderState
    updateReaderState p b ReaderState {readerIntersection, readerHead, readerId}
        = ReaderState
            { readerIntersection = updatePointer b p readerIntersection
            , readerHead         = updatePointer b p readerHead
            , readerId
            }

applyChainProducerUpdate :: ChainUpdate -> ChainProducerState -> ChainProducerState
applyChainProducerUpdate cu (cps@ChainProducerState {chainState})
    = (normalizeChainProducerState cu cps) { chainState = applyChainStateUpdate cu chainState }

invApplyChainProducerUpdate :: ChainUpdate -> ChainProducerState ->  Bool
invApplyChainProducerUpdate cu cps = case applyChainProducerUpdate cu cps of
    ChainProducerState
        { chainState    = ChainState (Volatile blocks _)
        , chainReaders
        } -> and
            [
              -- all pointers should be still in volatile chain
              and [ Map.member intersectionBlockId blocks && Map.member readerBlockId blocks
                  | ReaderState
                      { readerIntersection = (_, intersectionBlockId)
                      , readerHead         = (_, readerBlockId)
                      } <- chainReaders
                  ]
            ]

data ConsumeChain block = RollForward  block
                        | RollBackward Point

-- It does not validate if the new reader state is an improvment over the old
-- one.  If a node (an attacker) will force us to use older intersection point,
-- he will get longer instruction sets, so it's in its interest to send honest
-- information.
--
-- Assumptions:
--  * each reader is placed only once in `chainReaders` list (we update only the
--    first one in the list)
improveReaderState :: ChainProducerState
                   -> ReaderId
                   -> [Point] -- newest first
                   -> ChainProducerState
improveReaderState cps _ []  = cps
improveReaderState 
    (cps@ChainProducerState
        { chainState = ChainState (Volatile _ Nothing)
        })
    _ _ = cps
improveReaderState
    (cps@ChainProducerState
        { chainState   = ChainState (Volatile blocks (Just tip))
        , chainReaders = rs
        })
    rid ps = cps { chainReaders = updateReader rs }
    where
    updateReader :: [ReaderState] -> [ReaderState]
    updateReader [] = []
    updateReader (r : rs) =
        if readerId r == rid
            then go Nothing ps r : rs
            else r : updateReader rs

    go :: Maybe Point -> [Point] -> ReaderState -> ReaderState
    go _ []                  rs = rs
    go readerHead (point@(_, blockId) : ps') rs =
        let -- check if point is a new readerHead
            readerHead' = case readerHead of
                Just _  -> readerHead
                Nothing -> if Map.member blockId blocks
                            then Just point
                            else Nothing
        in case chainBackwardsFrom blocks blockId of
            []          -> go readerHead' ps' rs -- ^ blockId is not in volatile chain
            readersTine -> case chainBackwardsFrom blocks tip of
                []            -> go readerHead' ps' rs
                producersTine -> case readersTine `intersect_` producersTine of
                    Nothing                 -> go readerHead' ps' rs
                    Just readerIntersection -> ReaderState
                        { readerIntersection
                        , readerHead = fromMaybe point readerHead'
                        , readerId   = rid
                        }

    -- intersect two tines
    intersect_ :: [Block] -> [Block] -> Maybe Point
    intersect_ bs bs' =
        case map blockPoint bs `intersect` map blockPoint bs' of
            p : _ -> Just p
            []    -> Nothing

--TODO !
--
-- The general simulation propert for a suitablely constrained sequence of
-- the concrete operations.
--
-- The flush and prune operations allow quite a bit of flexibility about when
-- we do them, but there is a constraint that we flush before we prune so
-- that we do not break the chain overlap.
--
-- Could pick a specific flush policy but would like to check that an arbitrary
-- valid policy is still ok.

main :: IO ()
main = do
    quickCheck prop_TestChain
    quickCheck prop_TestChainAndUpdates
    quickCheck prop_emptyChainState
    quickCheck (\(TestVolatile v) -> invChainState (ChainState v))
    quickCheck prop_addBlock
    quickCheck prop_switchFork
