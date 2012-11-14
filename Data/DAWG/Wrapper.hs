-- | The module provides a wrapper over the 'D.DAWG' with separate
-- 'C.Codec' for values encoding, which is beneficial when the set
-- of possible DAWG values is small but individual values occupy
-- a lot of memory.
-- NOTE: Useless values are not deleted from the codec when
-- deleting the DAWG entry.

module Data.DAWG.Wrapper
( DAWG (..)
, empty
, numStates
, insert
, delete
, lookup
, fromList
, fromLang
) where

import Prelude hiding (lookup)
import Data.List (foldl')
import qualified Control.Monad.Codec as C
import qualified Data.DAWG as D

-- | A plain 'D.DAWG' with separate 'C.Codec' for values encoding.
data DAWG a = DAWG
    { dawg  :: !(D.DAWG Int)
    , codec :: !(C.AtomCodec a) }

-- | Empty DAWG.
empty :: DAWG a
empty = DAWG D.empty C.empty

-- | Number of states in the underlying graph.
numStates :: DAWG a -> Int
numStates = D.numStates . dawg

-- | Insert the (key, value) pair into the DAWG.
insert :: Ord a => String -> a -> DAWG a -> DAWG a
insert xs y d =
    let (y', c') = C.runCodec (codec d) (C.encode C.idLens y)
    in  DAWG (D.insert xs y' (dawg d)) c'

-- | Delete the key from the DAWG.
delete :: Ord a => String -> DAWG a -> DAWG a
delete xs d = DAWG (D.delete xs (dawg d)) (codec d)

-- | Find value associated with the key.
lookup :: Ord a => String -> DAWG a -> Maybe a
lookup xs d =
    D.lookup xs (dawg d) >>=
    C.evalCodec (codec d) . C.maybeDecode C.idLens

-- | Construct DAWG from the list of (word, value) pairs.
fromList :: Ord a => [(String, a)] -> DAWG a
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- | Make DAWG from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: [String] -> DAWG ()
fromLang xs = fromList [(x, ()) | x <- xs]