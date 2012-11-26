{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.

module Data.DAWG.Internal
(
-- * DAWG type
  DAWG (..)
-- * Query
, numStates
, lookup
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
import Data.Binary (Binary, put, get)
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.State.Strict as S

import Data.DAWG.Types
import Data.DAWG.Graph (Graph)
import Data.DAWG.Trans (Trans)
import qualified Data.DAWG.Trans as T
import qualified Data.DAWG.Trans.Vector as VT
import qualified Data.DAWG.Node as N
import qualified Data.DAWG.Graph as G

type Node t a = N.Node t (Maybe a) ()

class (Ord (Node t a), Trans t) => MkNode t a where
instance (Ord (Node t a), Trans t) => MkNode t a where

type GraphM t a b = S.State (Graph (Node t a)) b

mkState :: (Graph a -> Graph a) -> Graph a -> ((), Graph a)
mkState f g = ((), f g)

-- | Leaf node with no children and 'Nothing' value.
insertLeaf :: MkNode t a => GraphM t a ID
insertLeaf = do
    i <- insertNode (N.Leaf Nothing)
    insertNode (N.Branch i T.empty U.empty)

-- | Return node with the given identifier.
nodeBy :: ID -> GraphM t a (Node t a)
nodeBy i = G.nodeBy i <$> S.get

-- Evaluate the 'G.insert' function within the monad.
insertNode :: MkNode t a => Node t a -> GraphM t a ID
insertNode = S.state . G.insert

-- Evaluate the 'G.delete' function within the monad.
deleteNode :: MkNode t a => Node t a -> GraphM t a ()
deleteNode = S.state . mkState . G.delete

-- | Invariant: the identifier points to the 'Branch' node.
insertM :: MkNode t a => [Sym] -> a -> ID -> GraphM t a ID
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
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertNode (N.Leaf $ Just y)
    insertNode (n { N.eps = j })

insertWithM
    :: MkNode t a => (a -> a -> a)
    -> [Sym] -> a -> ID -> GraphM t a ID
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
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    let y'new = case N.value w of
            Just y' -> f y y'
            Nothing -> y
    j <- insertNode (N.Leaf $ Just y'new)
    insertNode (n { N.eps = j })

deleteM :: MkNode t a => [Sym] -> ID -> GraphM t a ID
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
    w <- nodeBy (N.eps n)
    deleteNode w
    deleteNode n
    j <- insertLeaf
    insertNode (n { N.eps = j })
    
lookupM :: Trans t => [Sym] -> ID -> GraphM t a (Maybe a)
lookupM [] i = do
    j <- N.eps <$> nodeBy i
    N.value <$> nodeBy j
lookupM (x:xs) i = do
    n <- nodeBy i
    case N.onSym x n of
        Just j  -> lookupM xs j
        Nothing -> return Nothing

assocsAcc :: Trans t => Graph (Node t a) -> ID -> [([Sym], a)]
assocsAcc g i =
    here w ++ concatMap there (N.edges n)
  where
    n = G.nodeBy i g
    w = G.nodeBy (N.eps n) g
    here v = case N.value v of
        Just x  -> [([], x)]
        Nothing -> []
    there (sym, j) = map (first (sym:)) (assocsAcc g j)

-- | A directed acyclic word graph with phantom type @a@ representing
-- type of alphabet elements.
data DAWG t a b = DAWG
    { graph :: !(Graph (Node t b))
    , root  :: !ID }
    deriving (Show)

deriving instance Eq b  => Eq  (DAWG VT.Trans a b)
deriving instance Ord b => Ord (DAWG VT.Trans a b)

instance (MkNode t b, Binary t, Binary b) => Binary (DAWG t a b) where
    put d = do
        put (graph d)
        put (root d)
    get = DAWG <$> get <*> get

-- | Empty DAWG.
empty :: (MkNode t b) => DAWG t a b
empty = 
    let (i, g) = S.runState insertLeaf G.empty
    in  DAWG g i

-- | Number of states in the underlying graph.
numStates :: DAWG t a b -> Int
numStates = G.size . graph

-- | Insert the (key, value) pair into the DAWG.
insert :: (Enum a, MkNode t b) => [a] -> b -> DAWG t a b -> DAWG t a b
insert xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertM xs y $ root d) (graph d)
    in  DAWG g i
{-# INLINE insert #-}
{-# SPECIALIZE insert
        :: (MkNode t b) => String -> b
        -> DAWG t Char b -> DAWG t Char b #-}

-- | Insert with a function, combining new value and old value.
-- 'insertWith' f key value d will insert the pair (key, value) into d if
-- key does not exist in the DAWG. If the key does exist, the function
-- will insert the pair (key, f new_value old_value).
insertWith
    :: (Enum a, MkNode t b) => (b -> b -> b)
    -> [a] -> b -> DAWG t a b -> DAWG t a b
insertWith f xs' y d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE insertWith
        :: MkNode t b => (b -> b -> b) -> String -> b
        -> DAWG t Char b -> DAWG t Char b #-}

-- | Delete the key from the DAWG.
delete :: (Enum a, MkNode t b) => [a] -> DAWG t a b -> DAWG t a b
delete xs' d =
    let xs = map fromEnum xs'
        (i, g) = S.runState (deleteM xs $ root d) (graph d)
    in  DAWG g i
{-# SPECIALIZE delete
        :: MkNode t b => String
        -> DAWG t Char b -> DAWG t Char b #-}

-- | Find value associated with the key.
lookup :: (Enum a, MkNode t b) => [a] -> DAWG t a b -> Maybe b
lookup xs' d =
    let xs = map fromEnum xs'
    in  S.evalState (lookupM xs $ root d) (graph d)
{-# SPECIALIZE lookup
        :: MkNode t b => String
        -> DAWG t Char b -> Maybe b #-}

-- | Return all key/value pairs in the DAWG in ascending key order.
assocs :: (Enum a, MkNode t b) => DAWG t a b -> [([a], b)]
assocs
    = map (first (map toEnum))
    . (assocsAcc <$> graph <*> root)
{-# SPECIALIZE assocs :: MkNode t b => DAWG t Char b -> [(String, b)] #-}

-- | Return all keys of the DAWG in ascending order.
keys :: (Enum a, MkNode t b) => DAWG t a b -> [[a]]
keys = map fst . assocs
{-# SPECIALIZE keys :: MkNode t b => DAWG t Char b -> [String] #-}

-- | Return all elements of the DAWG in the ascending order of their keys.
elems :: MkNode t b => DAWG t a b -> [b]
elems = map snd . (assocsAcc <$> graph <*> root)

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: (Enum a, MkNode t b) => [([a], b)] -> DAWG t a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs
{-# INLINE fromList #-}
{-# SPECIALIZE fromList
        :: MkNode t b => [(String, b)] -> DAWG t Char b #-}

-- | Construct DAWG from the list of (word, value) pairs
-- with a combining function.  The combining function is
-- applied strictly.
fromListWith
    :: (Enum a, MkNode t b) => (b -> b -> b)
    -> [([a], b)] -> DAWG t a b
fromListWith f xs =
    let update t (x, v) = insertWith f x v t
    in  foldl' update empty xs
{-# SPECIALIZE fromListWith
        :: MkNode t b => (b -> b -> b)
        -> [(String, b)] -> DAWG t Char b #-}

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: (Enum a, MkNode t ()) => [[a]] -> DAWG t a ()
fromLang xs = fromList [(x, ()) | x <- xs]
{-# SPECIALIZE fromLang :: MkNode t () => [String] -> DAWG t Char () #-}
