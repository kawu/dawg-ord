-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}


-- | Tests for the "Data.DAWG.Ord" module.


module Ord where


import qualified Data.Set as S
import qualified Data.Map.Strict as M

import           Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.SmallCheck.Series as SC
import qualified Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import qualified Data.DAWG.Ord as D


tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]


scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "id == assocs . fromList (up to order)" $
        \xs -> M.fromList (D.assocs (D.fromList xs))
            == M.fromList (xs :: [(String, Int)])
    , SC.testProperty
        "Number of states and transitions in a \"full\" langauge" $
          SC.changeDepth (+1) $ \n k ->
            let dawg = D.fromLang (genFull n k)
            in  D.numStates dawg == SC.getNonNegative k + 1 &&
                D.numEdges  dawg ==
                    SC.getPositive n * SC.getNonNegative k
    ]


qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "id == assocs . fromList (up to order)" $
        \xs -> M.fromList (D.assocs (D.fromList xs))
            == M.fromList (xs :: [(String, Int)])
    ]


unitTests = testGroup "Unit tests"
    [ testCase "Size of a DAWG build from sample data" $ do
        let dawg = D.fromList dataSet1
        D.numStates dawg @?= 11
        D.numEdges dawg @?= 12
    ]


---------------------------------------------------------------------
-- Sample Data
---------------------------------------------------------------------


-- | Sample dataset no. 1.  See also `unitTests`.
dataSet1 :: [(String, Int)]
dataSet1 =
    [ ("asdf", 1)
    , ("asd", 1)
    , ("adf", 1)
    , ("df", 1)
    , ("asdfg", 3)
    , ("sdfg", 3) ]


---------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------


-- | Generate a \"full\" language of words of length `k` over
-- an alphabet of size `n`.
genFull :: SC.Positive Int -> SC.NonNegative Int -> [[Int]]
genFull (SC.Positive n) (SC.NonNegative k) =
    genLang n k
  where
    genLang n 0 = [[]]
    genLang n k =
        [ x:xs
        | xs <- genLang n (k - 1)
        , x <- [1 .. n]]
