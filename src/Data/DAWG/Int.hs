-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides a fast insert operation which can be
-- used to build the DAWG structure incrementaly.
--
-- Keys and values must provide an 'Enum' instance; see the
-- 'Data.DAWG.Ord' module if you look for a more generic solution.


module Data.DAWG.Int
(
-- * DAWG type
  DAWG
, ID
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
