-- | Basic types used throughout the library.

module Data.DAWG.Gen.Types
( ID
, Sym
, Val
) where

-- | Node identifier.
type ID = Int

-- | Internal representation of an alphabet element.
type Sym = Int

-- | Internal representation of an automaton value.
type Val = Int
