-- | A version of `Data.DAWG.Int` adapted to words with `Ord`
-- instances.


module Data.DAWG.Ord
(
-- * DAWG type
  DAWG
, ID
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


import           Data.DAWG.Gen.Types
import           Data.DAWG.Ord.Dynamic
