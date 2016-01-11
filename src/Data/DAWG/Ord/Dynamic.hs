{-# LANGUAGE RecordWildCards #-}


-- | A version of "Data.DAWG.Int.Dynamic" adapted to
-- keys and values with 'Ord' instances.


module Data.DAWG.Ord.Dynamic
(
-- * DAWG type
  DAWG
, root

-- * Query
, lookup
, numStates
, numEdges

-- * Traversal
, value
, edges
, follow

-- * Construction
, empty
, fromList
, fromLang
-- ** Insertion
, insert

-- * Conversion
, assocs
, keys
, elems
) where


import           Prelude hiding (lookup)
import           Data.List (foldl')
import           Control.Arrow (first)
import qualified Control.Monad.State.Strict as S

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import           Data.DAWG.Gen.Types
import qualified Data.DAWG.Int.Dynamic as D


------------------------------------------------------------
-- DAWG
------------------------------------------------------------


-- | A directed acyclic word graph (DAWG) with type @a@ representing
-- the type of alphabet symbols (over which keys are constructed)
-- and type @b@ -- the type of values.
--
-- A DAWG can be seen as a map from keys (sequences of @a@'s) to
-- values @b@.
data DAWG a b = DAWG
    { intDAWG   :: D.DAWG
    , symMap    :: M.Map a Int
    , symMapR   :: M.Map Int a
    , valMap    :: M.Map b Int
    , valMapR   :: M.Map Int b
    } deriving (Show, Eq, Ord)


-- | The root (start state) of the DAWG.
root :: DAWG a b -> ID
root = D.root . intDAWG


------------------------------------------------------------
-- State monad over the underlying DAWG
------------------------------------------------------------


-- | DAWG monad.
type DM a b = S.State (DAWG a b)


-- | Register new key in the underlying automaton.
-- TODO: We could optimize it.
addSym :: Ord a => a -> DM a b Int
addSym x = S.state $ \dawg@DAWG{..} ->
    let y = fromMaybe (M.size symMap) (M.lookup x symMap)
--     let y = case M.lookup x symMap of
--             Nothing -> M.size symMap
--             Just k  -> k
    in  (y, dawg
            { symMap  = M.insert x y symMap
            , symMapR = M.insert y x symMapR })


-- | Register new key in the underlying automaton.
addKey :: Ord a => [a] -> DM a b [Int]
addKey = mapM addSym


-- | Register new value in the underlying automaton.
-- TODO: We could optimize it.
addVal :: Ord b => b -> DM a b Int
addVal x = S.state $ \dawg@DAWG{..} ->
    let y = fromMaybe (M.size valMap) (M.lookup x valMap)
--     let y = case M.lookup x valMap of
--             Nothing -> M.size valMap
--             Just k  -> k
    in  (y, dawg
            { valMap  = M.insert x y valMap
            , valMapR = M.insert y x valMapR })

-- | Run the DAGW monad.
runDM :: DM a b c -> DAWG a b -> (c, DAWG a b)
runDM = S.runState


------------------------------------------------------------
-- The proper DAWG interface
------------------------------------------------------------


-- | Empty DAWG.
empty :: DAWG a b
empty = DAWG D.empty M.empty M.empty M.empty M.empty


-- | Number of states in the underlying automaton.
numStates :: DAWG a b -> Int
numStates = D.numStates . intDAWG


-- | Number of transitions in the underlying automaton.
numEdges :: DAWG a b -> Int
numEdges = D.numEdges . intDAWG


-- | Insert the (key, value) pair into the DAWG.
insert :: (Ord a, Ord b) => [a] -> b -> DAWG a b -> DAWG a b
insert xs0 y0 dag0 = snd $ flip runDM dag0 $ do
    xs <- addKey xs0
    y  <- addVal y0
    S.modify $ \dag -> dag
        {intDAWG = D.insert xs y (intDAWG dag)}


-- -- | Insert with a function, combining new value and old value.
-- -- 'insertWith' f key value d will insert the pair (key, value) into d if
-- -- key does not exist in the DAWG. If the key does exist, the function
-- -- will insert the pair (key, f new_value old_value).
-- insertWith
--     :: (Ord a, Ord b) => (b -> b -> b)
--     -> [a] -> b -> DAWG a b -> DAWG a b
-- insertWith f xs y dag =
--     let y' = lookup xs dag
--     in  insert xs (f y y') dag


-- -- | Delete the key from the DAWG.
-- delete :: (Enum a, Ord b) => [a] -> DAWG a b -> DAWG a b
-- delete xs' d =
--     let xs = map fromEnum xs'
--         (i, g) = S.runState (deleteM xs $ root d) (graph d)
--     in  DAWG g i
-- {-# SPECIALIZE delete :: Ord b => String -> DAWG Char b -> DAWG Char b #-}


-- | Find value associated with the key.
lookup :: (Ord a, Ord b) => [a] -> DAWG a b -> Maybe b
lookup xs0 DAWG{..} = do
    xs <- mapM (`M.lookup` symMap) xs0
    y  <- D.lookup xs intDAWG
    M.lookup y valMapR


-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: DAWG a b -> [([a], b)]
assocs DAWG{..} =
    [ (decodeKey xs, decodeVal y)
    | (xs, y) <- D.assocs intDAWG ]
  where
    decodeKey = map decodeSym
    decodeSym x = symMapR M.! x
    decodeVal x = valMapR M.! x


-- | Return all keys of the DAWG in ascending order.
keys :: DAWG a b -> [[a]]
keys = map fst . assocs


-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: DAWG a b -> [b]
elems = map snd . assocs


-- | Construct DAWG from the list of (key, value) pairs.
fromList :: (Ord a, Ord b) => [([a], b)] -> DAWG a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs


-- | Make DAWG from the list of words (annotate each word with
-- the @()@ value).
fromLang :: Ord a => [[a]] -> DAWG a ()
fromLang xs = fromList [(x, ()) | x <- xs]


------------------------------------------------------------
-- Traversal
------------------------------------------------------------


-- | Value stored in the given automaton state.
value :: ID -> DAWG a b -> Maybe b
value i DAWG{..}  = do
    x <- D.value i intDAWG
    M.lookup x valMapR


-- | A list of outgoing edges (automaton transitions).
edges :: ID -> DAWG a b -> [(a, ID)]
edges i DAWG{..} = map
    (first (symMapR M.!))
    (D.edges i intDAWG)


-- | Follow a transition with the given symbol from the given state.
follow :: Ord a => ID -> a -> DAWG a b -> Maybe ID
follow i x DAWG{..} = do
    y <- M.lookup x symMap
    D.follow i y intDAWG
