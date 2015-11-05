{-# LANGUAGE RecordWildCards #-}


-- | Internal representation of a static automata node.


module Data.DAWG.Int.Static.Node
( Node(..)
, onSym
, onSym'
, edges
, children
, insert
, fromDyn
) where


import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Gen.Types
import Data.DAWG.Gen.Trans.Vector (Trans)
import qualified Data.DAWG.Gen.Trans as T
import qualified Data.DAWG.Int.Dynamic.Node as D


-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they must be represented as one node in the graph)
-- iff they are equal with respect to their values and outgoing
-- edges.
data Node a = Node {
    -- | Epsilon transition.
      value     :: !(Maybe Val)
    -- | Transition map (outgoing edges).
    , transMap  :: !Trans
    -- | Labels corresponding to individual edges.
    -- TODO: Why do we need it!?
    , labelVect :: !(U.Vector a)
    } deriving (Show, Eq, Ord)

instance (U.Unbox a, Binary a) => Binary (Node a) where
    put Node{..} = put value >> put transMap >> put labelVect
    get = Node <$> get <*> get <*> get


-- | Transition function.
onSym :: Sym -> Node a -> Maybe ID
onSym x (Node _ t _)  = T.lookup x t
{-# INLINE onSym #-}


-- | Transition function.
onSym' :: U.Unbox a => Sym -> Node a -> Maybe (ID, a)
onSym' x (Node _ t ls)   = do
    k <- T.index x t
    (,) <$> (snd <$> T.byIndex k t)
        <*> ls U.!? k
{-# INLINE onSym' #-}


-- | List of symbol/edge pairs outgoing from the node.
edges :: Node a -> [(Sym, ID)]
edges (Node _ t _)    = T.toList t
{-# INLINE edges #-}

-- | List of children identifiers.
children :: Node a -> [ID]
children = map snd . edges
{-# INLINE children #-}


-- | Substitue edge determined by a given symbol.
insert :: Sym -> ID -> Node a -> Node a
insert x i (Node w t ls)  = Node w (T.insert x i t) ls
{-# INLINE insert #-}


-- | Make "static" node from a "dynamic" node.
fromDyn
    :: (ID -> ID)   -- ^ Assign new IDs 
    -> D.Node       -- ^ "Dynamic" node
    -> Node ()      -- ^ "Static" node
fromDyn f (D.Node v t)    =
    let reTrans = T.fromList . map (second f) . T.toList
    in  Node v (reTrans t) U.empty
