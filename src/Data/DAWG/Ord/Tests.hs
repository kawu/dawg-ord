-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}


module Data.DAWG.Ord.Tests where


import qualified Data.Set as S

import           Test.Tasty (TestTree, testGroup)
import           Test.HUnit (Assertion, (@?=))
import           Test.Tasty.HUnit (testCase)

import qualified Data.DAWG.Ord as D


---------------------------------------------------------------------
-- All Tests
---------------------------------------------------------------------


-- | All tests of the corresponding module.
tests :: TestTree
tests = testGroup "Data.DAWG.Ord"
    [ testCase "fullLang" $ fullLang [0..2::Int] 5
    , testCase "assocsFromListID" $ assocsFromListID dataSet1
    , testCase "dataSet1DAWGSize" dataSet1DAWGSize ]


---------------------------------------------------------------------
-- Assertions / Properties
---------------------------------------------------------------------


-- | Verify that `assocs . fromList` is an identity (up to the order
-- of the elements in the input list).
assocsFromListID
    :: (Show a, Ord a, Show b, Ord b)
    => [([a], b)]
    -> Assertion
assocsFromListID xs
    =   S.fromList (D.assocs (D.fromList xs))
    @?= S.fromList xs


-- | Verify the numbers of states and transitions for a language S^n
-- where S is an non-empty alphabet and n specifies the length of
-- words in the language.  Both S and n are provided as input.
fullLang :: Ord a => [a] -> Int -> Assertion
fullLang xList n = do
    D.numStates dawg @?= n + 1
    D.numEdges dawg @?= (S.size xSet * n)
  where
    xSet = S.fromList xList
    dawg = D.fromLang (genLang n)
    genLang 0 = [[]]
    genLang k = [ x:xs
                | xs <- genLang (k - 1)
                , x <- S.toList xSet]


-- | Verify the size of the DAWG constructed from dataset no. 1.
dataSet1DAWGSize :: Assertion
dataSet1DAWGSize = do
    D.numStates dawg @?= 11
    D.numEdges dawg @?= 12
  where
    dawg = D.fromList dataSet1


---------------------------------------------------------------------
-- Sample Data
---------------------------------------------------------------------


-- | Sample dataset no. 1.  See also `dataSet1DAWGSize`.
dataSet1 :: [(String, Int)]
dataSet1 =
    [ ("asdf", 1)
    , ("asd", 1)
    , ("adf", 1)
    , ("df", 1)
    , ("asdfg", 3)
    , ("sdfg", 3) ]
