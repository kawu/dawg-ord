-- | A version of `Data.DAWG.Int` adapted to keys and values with
-- `Ord` instances.


module Data.DAWG.Ord
(
-- * DAWG type
  DAWG
, ID
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

import           Data.DAWG.Gen.Types
import           Data.DAWG.Ord.Dynamic
