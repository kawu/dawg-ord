{-# LANGUAGE RecordWildCards #-}


-- | A version of `Data.DAWG.Int.Dynamic` adapted to words with `Ord`
-- instances.


module Data.DAWG.Ord.Dynamic
(
-- * DAWG type
  DAWG
, root

-- * Query
, member
, numStates
, numEdges

-- * Traversal
, accept
, edges
, follow

-- * Construction
, empty
, fromList
-- ** Insertion
, insert

-- * Conversion
, keys
) where


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


-- | A directed acyclic word graph with type `a` representing the
-- type of alphabet elements.
data DAWG a = DAWG
    { intDAWG   :: D.DAWG Sym
    , symMap    :: M.Map a Int
    , symMapR   :: M.Map Int a
    } deriving (Show, Eq, Ord)


-- | Root of the DAWG.
root :: DAWG a -> ID
root = D.root . intDAWG


------------------------------------------------------------
-- State monad over the underlying DAWG
------------------------------------------------------------


-- | DAWG monad.
type DM a = S.State (DAWG a)


-- | Register new key in the underlying automaton.
-- TODO: We could optimize it.
addSym :: Ord a => a -> DM a Int
addSym x = S.state $ \dawg@DAWG{..} ->
    let y = fromMaybe (M.size symMap) (M.lookup x symMap)
--     let y = case M.lookup x symMap of
--             Nothing -> M.size symMap
--             Just k  -> k
    in  (y, dawg
            { symMap  = M.insert x y symMap
            , symMapR = M.insert y x symMapR })


-- | Register new key in the underlying automaton.
addKey :: Ord a => [a] -> DM a [Int]
addKey = mapM addSym


-- | Run the DAGW monad.
runDM :: DM a c -> DAWG a -> (c, DAWG a)
runDM = S.runState


------------------------------------------------------------
-- The proper DAWG interface
------------------------------------------------------------


-- | Empty DAWG.
empty :: DAWG a
empty = DAWG D.empty M.empty M.empty


-- | Number of states in the automaton.
numStates :: DAWG a -> Int
numStates = D.numStates . intDAWG


-- | Number of edges in the automaton.
numEdges :: DAWG a -> Int
numEdges = D.numEdges . intDAWG


-- | Insert the word into the DAWG.
insert :: (Ord a) => [a] -> DAWG a -> DAWG a
insert xs0 dag0 = snd $ flip runDM dag0 $ do
    xs <- addKey xs0
    S.modify $ \dag -> dag
        {intDAWG = D.insert xs (intDAWG dag)}


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


-- | Is the word a member of the DAWG?
member :: (Ord a) => [a] -> DAWG a -> Bool
member xs0 DAWG{..} = justTrue $ do
    xs <- mapM (`M.lookup` symMap) xs0
    return $ D.member xs intDAWG


-- | Return all keys in the DAWG in ascending key order.
keys :: DAWG a -> [[a]]
keys DAWG{..} =
    [ decodeKey xs
    | xs <- D.keys intDAWG ]
  where
    decodeKey = map decodeSym
    decodeSym x = symMapR M.! x


-- | Construct DAWG from the list of words.
fromList :: (Ord a) => [[a]] -> DAWG a
fromList xs =
    let update t x = insert x t
    in  foldl' update empty xs


------------------------------------------------------------
-- Traversal
------------------------------------------------------------


-- | Does the identifer represent an accepting state?
accept :: ID -> DAWG a -> Bool
accept i DAWG{..} = D.accept i intDAWG


-- | A list of outgoing edges.
edges :: ID -> DAWG a -> [(a, ID)]
edges i DAWG{..} = map
    (first (symMapR M.!))
    (D.edges i intDAWG)


-- | Follow the given transition from the given state.
follow :: Ord a => ID -> a -> DAWG a -> Maybe ID
follow i x DAWG{..} = do
    y <- M.lookup x symMap
    D.follow i y intDAWG


------------------------------------------------------------
-- Misc
------------------------------------------------------------


-- | Is it `Just True`?
justTrue :: Maybe Bool -> Bool
justTrue (Just True) = True
justTrue _           = False
