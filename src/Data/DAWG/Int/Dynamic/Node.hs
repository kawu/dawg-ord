{-# LANGUAGE RecordWildCards #-}


-- | Internal representation of dynamic automata nodes.


module Data.DAWG.Int.Dynamic.Node
( Node(..)
, onSym
, edges
, children
, insert
) where


-- import Control.Applicative ((<$>), (<*>))
-- import Data.Binary (Binary, put, get)

import Data.DAWG.Gen.Types
import Data.DAWG.Gen.Util (combine)
import Data.DAWG.Gen.HashMap (Hash, hash)
import Data.DAWG.Gen.Trans.Map (Trans)
import qualified Data.DAWG.Gen.Trans as T
import qualified Data.DAWG.Gen.Trans.Hashed as H


-- | Two nodes (states) belong to the same equivalence class (and,
-- consequently, they must be represented as one node in the graph)
-- iff they are equal with respect to their values and outgoing
-- edges.
data Node = Node {
    -- | Accepting state or no?
      accept    :: !Bool
    -- | Transition map (outgoing edges).
    , transMap :: !(H.Hashed Trans)
    } deriving (Show, Eq, Ord)

instance Hash Node where
    hash Node{..} = combine (hash accept) (H.hash transMap)

-- instance Binary Node where
--     put Node{..} = put accept >> put transMap
--     get = Node <$> get <*> get


-- | Transition function.
onSym :: Sym -> Node -> Maybe ID
onSym x (Node _ t)    = T.lookup x t
{-# INLINE onSym #-}


-- | List of symbol/edge pairs outgoing from the node.
edges :: Node -> [(Sym, ID)]
edges (Node _ t)  = T.toList t
{-# INLINE edges #-}


-- | List of children identifiers.
children :: Node -> [ID]
children = map snd . edges
{-# INLINE children #-}


-- | Substitue edge determined by a given symbol.
insert :: Sym -> ID -> Node -> Node
insert x i (Node a t) = Node a (T.insert x i t)
{-# INLINE insert #-}
