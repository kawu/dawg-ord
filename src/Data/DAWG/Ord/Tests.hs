{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Data.DAWG.Ord.Tests where


import qualified Data.Set as S

import           Test.Tasty (TestTree, testGroup)
import           Test.HUnit (Assertion, (@?=))
import           Test.Tasty.HUnit (testCase)

import qualified Data.DAWG.Ord as D


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


-- | All tests of the corresponding module.
tests :: TestTree
tests = testGroup "Data.DAWG.Ord"
    [ testCase "fullLang" $ fullLang [0..2::Int] 5 ]
