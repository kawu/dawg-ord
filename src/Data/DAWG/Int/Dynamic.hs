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
, fromListWith
, fromLang
-- ** Insertion
, insert
, insertWith
-- ** Deletion
, delete

-- * Conversion
, assocs
, keys
, elems
) where


import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')
import qualified Control.Monad.State.Strict as S
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class

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
insertLeaf = insertNode $ N.Node Nothing T.empty
    -- i <- insertNode (N.Leaf Nothing)
    -- insertNode (N.Branch i T.empty)


-- Evaluate the 'G.delete' function within the monad.
deleteNode ::  N.Node -> GraphM ()
deleteNode = S.state . mkState . G.delete


-- | Invariant: the identifier points to the 'Branch' node.
insertM :: [Sym] -> Val -> ID -> GraphM ID
insertM (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertM xs y j
    deleteNode n
    insertNode (N.insert x k n)
insertM [] y i = do
    n <- nodeBy i
    deleteNode n
    insertNode (n { N.value = Just y })


insertWithM
    :: (Val -> Val -> Val)
    -> [Sym] -> Val -> ID -> GraphM ID
insertWithM f (x:xs) y i = do
    n <- nodeBy i
    j <- case N.onSym x n of
        Just j  -> return j
        Nothing -> insertLeaf
    k <- insertWithM f xs y j
    deleteNode n
    insertNode (N.insert x k n)
insertWithM f [] y i = do
    n <- nodeBy i
    deleteNode n
    let y'new = case N.value n of
            Just y' -> f y y'
            Nothing -> y
    insertNode (n { N.value = Just y'new })


deleteM :: [Sym] -> ID -> GraphM ID
deleteM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Nothing -> return i
        Just j  -> do
            k <- deleteM xs j
            deleteNode n
            insertNode (N.insert x k n)
deleteM [] i = do
    n <- nodeBy i
    deleteNode n
    insertNode (n { N.value = Nothing })


-- | Follow the path from the given identifier.
followPath :: [Sym] -> ID -> MaybeT GraphM ID
followPath (x:xs) i = do
    n <- lift $ nodeBy i
    j <- liftMaybe $ N.onSym x n
    followPath xs j
followPath [] i = return i
    

lookupM :: [Sym] -> ID -> GraphM (Maybe Val)
lookupM xs i = runMaybeT $ do
    j <- followPath xs i
    MaybeT $ N.value <$> nodeBy j


------------------------------------------------------------
-- The proper DAWG interface
------------------------------------------------------------


-- | Return all (key, value) pairs in ascending key order in the
-- sub-DAWG determined by the given node ID.
subPairs :: Graph N.Node -> ID -> [([Sym], Val)]
subPairs g i =
    here n ++ concatMap there (N.edges n)
  where
    n = G.nodeBy i g
    here v = case N.value v of
        Just x  -> [([], x)]
        Nothing -> []
    there (sym, j) = map (first (sym:)) (subPairs g j)


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
insert :: Enum a => [a] -> Val -> DAWG a -> DAWG a
insert xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i
{-# INLINE insert #-}


-- | Insert with a function, combining new value and old value.
-- 'insertWith' f key value d will insert the pair (key, value) into d if
-- key does not exist in the DAWG. If the key does exist, the function
-- will insert the pair (key, f new_value old_value).
insertWith
    :: Enum a => (Val -> Val -> Val)
    -> [a] -> Val -> DAWG a -> DAWG a
insertWith f xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE insertWith
        :: (Val -> Val -> Val) -> String -> Val
        -> DAWG Char -> DAWG Char #-}


-- | Delete the key from the DAWG.
delete :: Enum a => [a] -> DAWG a -> DAWG a
delete xs' d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (deleteM xs $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE delete :: String -> DAWG Char -> DAWG Char #-}


-- | Find value associated with the key.
lookup :: Enum a => [a] -> DAWG a -> Maybe Val
lookup xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (lookupM xs $ root d) (graph d)
{-# SPECIALIZE lookup :: String -> DAWG Char -> Maybe Val #-}


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
assocs :: Enum a => DAWG a -> [([a], Val)]
assocs
    = map (first (map toEnum))
    . (subPairs <$> graph <*> root)
{-# SPECIALIZE assocs :: DAWG Char -> [(String, Val)] #-}


-- | Return all keys of the DAWG in ascending order.
keys :: Enum a => DAWG a -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: DAWG Char -> [String] #-}


-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: DAWG a -> [Val]
elems = map snd . (subPairs <$> graph <*> root)


-- | Construct DAWG from the list of (word, value) pairs.
fromList :: Enum a => [([a], Val)] -> DAWG a
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs
{-# INLINE fromList #-}


-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly.
fromListWith
    :: Enum a => (Val -> Val -> Val)
    -> [([a], Val)] -> DAWG a
fromListWith f xs =
    let update t (x, v) = insertWith f x v t
    in  foldl' update empty xs
{-# SPECIALIZE fromListWith
        :: (Val -> Val -> Val)
        -> [(String, Val)] -> DAWG Char #-}


-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: Enum a => [[a]] -> DAWG a
fromLang xs = fromList [(x, 0) | x <- xs]
{-# SPECIALIZE fromLang :: [String] -> DAWG Char #-}


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
value :: ID -> DAWG a -> Maybe Val
value i = N.value . G.nodeBy i . graph


-- | Follow the given transition from the given state.
follow :: Enum a => ID -> a -> DAWG a -> Maybe ID
follow i x DAWG{..} = flip S.evalState graph $ runMaybeT $
    followPath [fromEnum x] i


------------------------------------------------------------
-- Misc
------------------------------------------------------------


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
{-# INLINE liftMaybe #-}
