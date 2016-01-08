-- | The module implements /directed acyclic word graphs/ (DAWGs)
-- internaly represented as /minimal acyclic deterministic
-- finite-state automata/.
-- The implementation provides a fast insert operation which can be
-- used to build the DAWG structure incrementaly.
--
-- Alphabet symbols must have an `Enum` instance; see `Data.DAWG.Ord`
-- if you look for a more generic solution.


module Data.DAWG.Int
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
import           Data.DAWG.Int.Dynamic
