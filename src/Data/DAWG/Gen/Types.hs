-- | Basic types used throughout the library.

module Data.DAWG.Gen.Types
( ID
, Sym
, Val
) where

-- | Identifier of a DAWG node (automaton state).
type ID = Int

-- | A transition symbol.
type Sym = Int

-- | A type of DAWG values, stored in accept states.
type Val = Int
