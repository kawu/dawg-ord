-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.
--
-- See the "Data.DAWG.Ord" module if you look for a more generic
-- solution (which, for the moment, lacks some of the functionality provided
-- here, e.g. the `delete` function).


module Data.DAWG.Int
(
-- * DAWG type
  DAWG
, ID
, Sym
, Val
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


import           Prelude hiding (lookup)

import           Data.DAWG.Gen.Types
import           Data.DAWG.Int.Dynamic
