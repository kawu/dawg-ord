{-# LANGUAGE RecordWildCards #-}


-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.


module Data.DAWG.Int.Dynamic
(
-- * DAWG type
  DAWG (root)

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
-- ** Deletion
-- , delete

-- * Conversion
, keys
) where


import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import qualified Control.Monad.State.Strict as S
-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.Class

import           Data.DAWG.Gen.Types
import           Data.DAWG.Gen.Graph (Graph)
import qualified Data.DAWG.Gen.Trans as T
import qualified Data.DAWG.Gen.Graph as G
import           Data.DAWG.Int.Dynamic.Internal
import qualified Data.DAWG.Int.Dynamic.Node as N


------------------------------------------------------------
-- State monad over the underlying graph
------------------------------------------------------------


type GraphM = S.State (Graph N.Node)


-- | A utility function to run in cooperation with `S.state`.
mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)


-- | Return node with the given identifier.
nodeBy :: ID -> GraphM N.Node
nodeBy i = G.nodeBy i <$> S.get


-- Evaluate the 'G.insert' function within the monad.
insertNode :: N.Node -> GraphM ID
insertNode = S.state . G.insert


-- | Leaf node with no children and 'Nothing' value.
insertLeaf :: GraphM ID
insertLeaf = insertNode $ N.Node False T.empty
    -- i <- insertNode (N.Leaf Nothing)
    -- insertNode (N.Branch i T.empty)


-- Evaluate the 'G.delete' function within the monad.
deleteNode ::  N.Node -> GraphM ()
deleteNode = S.state . mkState . G.delete


-- | Invariant: the identifier points to the 'Branch' node.
-- TODO: which identifier?
insertM :: [Sym] -> ID -> GraphM ID
insertM (x:xs) i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertM xs j
    deleteNode n
    insertNode (N.insert x k n)
insertM [] i = do
    n <- nodeBy i
    deleteNode n
    insertNode (n { N.accept = True })


-- deleteM :: [Sym] -> ID -> GraphM ID
-- deleteM (x:xs) i = do
--     n <- nodeBy i
--     case N.onSym x n of
--         Nothing -> return i
--         Just j  -> do
--             k <- deleteM xs j
--             deleteNode n
--             insertNode (N.insert x k n)
-- deleteM [] i = do
--     n <- nodeBy i
--     deleteNode n
--     insertNode (n { N.value = Nothing })


-- -- | Follow the path from the given identifier.
-- followPath :: [Sym] -> ID -> MaybeT GraphM ID
-- followPath (x:xs) i = do
--     n <- lift $ nodeBy i
--     j <- liftMaybe $ N.onSym x n
--     followPath xs j
-- followPath [] i = return i


-- | Follow the path from the given identifier.
followPath' :: [Sym] -> ID -> GraphM (Maybe ID)
followPath' (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
         Nothing -> return Nothing
         Just j  -> followPath' xs j
followPath' [] i = return $ Just i


memberM :: [Sym] -> ID -> GraphM Bool
memberM xs i = do
    mj <- followPath' xs i
    case mj of
         Nothing    -> return False
         Just j     -> N.accept <$> nodeBy j


-- memberM :: [Sym] -> ID -> GraphM Bool
-- memberM xs i = fmap justTrue . runMaybeT $ do
--     j <- followPath xs i
--     lift $ N.accept <$> nodeBy j
--   where
--     justTrue (Just True) = True
--     justTrue _           = False


------------------------------------------------------------
-- The proper DAWG interface
------------------------------------------------------------


-- | Return all (key, value) pairs in ascending key order in the
-- sub-DAWG determined by the given node ID.
subPairs :: Graph N.Node -> ID -> [[Sym]]
subPairs g i =
    here n ++ concatMap there (N.edges n)
  where
    n = G.nodeBy i g
    here v = [[] | N.accept v]
--     here v = if N.accept v
--         then [[]]
--         else []
    there (sym, j) = map (sym:) (subPairs g j)


-- | Empty DAWG.
empty :: DAWG a
empty =
    let (i, g) = S.runState insertLeaf G.empty
    in  DAWG g i


-- | Number of states in the automaton.
numStates :: DAWG a -> Int
numStates = G.size . graph


-- | Number of edges in the automaton.
numEdges :: DAWG a -> Int
numEdges = sum . map (length . N.edges) . G.nodes . graph


-- | Insert the (key, value) pair into the DAWG.
insert :: Enum a => [a] -> DAWG a -> DAWG a
insert xs' d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertM xs $ root d) (graph d)
    in  DAWG g i
{-# INLINE insert #-}


-- -- | Delete the key from the DAWG.
-- delete :: Enum a => [a] -> DAWG a -> DAWG a
-- delete xs' d =
--     let xs = map fromEnum xs'
--         (i, g) = S.runState (deleteM xs $ root d) (graph d)
--     in  DAWG g i
-- {-# SPECIALIZE delete :: String -> DAWG Char -> DAWG Char #-}


-- | Find value associated with the key.
member :: Enum a => [a] -> DAWG a -> Bool
member xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (memberM xs $ root d) (graph d)
{-# SPECIALIZE member :: String -> DAWG Char -> Bool #-}


-- -- | Find all (key, value) pairs such that key is prefixed
-- -- with the given string.
-- withPrefix :: (Enum a, Ord b) => [a] -> DAWG a b -> [([a], b)]
-- withPrefix xs DAWG{..}
--     = map (first $ (xs ++) . map toEnum)
--     $ maybe [] (subPairs graph)
--     $ flip S.evalState graph $ runMaybeT
--     $ follow (map fromEnum xs) root
-- {-# SPECIALIZE withPrefix
--     :: Ord b => String -> DAWG Char b
--     -> [(String, b)] #-}


-- | Return all key/value pairs in the DAWG in ascending key order.
keys :: Enum a => DAWG a -> [[a]]
keys
    = map (map toEnum)
    . (subPairs <$> graph <*> root)
{-# SPECIALIZE keys :: DAWG Char -> [String] #-}


-- | Construct DAWG from the list of (word, value) pairs.
fromList :: Enum a => [[a]] -> DAWG a
fromList xs =
    let update t x = insert x t
    in  foldl' update empty xs
{-# SPECIALIZE fromList :: [String] -> DAWG Char #-}


------------------------------------------------------------
-- Traversal
------------------------------------------------------------


-- | A list of outgoing edges.
edges :: Enum a => ID -> DAWG a -> [(a, ID)]
edges i
    = map (first toEnum)
    . N.edges . G.nodeBy i
    . graph
{-# SPECIALIZE edges :: ID -> DAWG Char -> [(Char, ID)] #-}
{-# SPECIALIZE edges :: ID -> DAWG Int  -> [(Int, ID)]  #-}


-- | Value stored in the given state.
accept :: ID -> DAWG a -> Bool
accept i = N.accept . G.nodeBy i . graph


-- -- | Follow the given transition from the given state.
-- follow :: Enum a => ID -> a -> DAWG a -> Maybe ID
-- follow i x DAWG{..} = flip S.evalState graph $ runMaybeT $
--     followPath [fromEnum x] i


-- | Follow the given transition from the given state.
follow :: Enum a => ID -> a -> DAWG a -> Maybe ID
follow i x DAWG{..} = flip S.evalState graph $
    followPath' [fromEnum x] i


------------------------------------------------------------
-- Misc
------------------------------------------------------------


-- liftMaybe :: Monad m => Maybe a -> MaybeT m a
-- liftMaybe = MaybeT . return
-- {-# INLINE liftMaybe #-}
