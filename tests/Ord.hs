{-# LANGUAGE FlexibleContexts #-}


-- | Tests for the "Data.DAWG.Ord" module.


module Ord where


import qualified Control.Monad.State.Strict as E
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
    , SC.testProperty "Actual number of transitions == D.numEdges" $
        \xs -> let dawg = D.fromList (xs :: [(String, Int)])
                in S.size (walk dawg) == D.numEdges dawg
    , SC.testProperty "Actual number of states == D.numStates" $
        \xs -> let dawg = D.fromList (xs :: [(String, Int)])
                in D.numStates dawg ==
                    S.size (states (D.root dawg) (walk dawg))
    ]


qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "id == assocs . fromList (up to order)" $
        \xs -> M.fromList (D.assocs (D.fromList xs))
            == M.fromList (xs :: [(String, Int)])
    , QC.testProperty "Actual number of transitions == D.numEdges" $
        \xs -> let dawg = D.fromList (xs :: [(String, Int)])
                in S.size (walk dawg) == D.numEdges dawg
    , QC.testProperty "Actual number of states == D.numStates" $
        \xs -> let dawg = D.fromList (xs :: [(String, Int)])
                in D.numStates dawg ==
                    S.size (states (D.root dawg) (walk dawg))
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


-- | Traverse the automaton and collect all the transitions.
walk :: Ord a => D.DAWG a b -> S.Set (D.ID, a, D.ID)
walk dawg =
    flip E.execState S.empty $
        flip E.evalStateT S.empty $
            doit (D.root dawg)
  where
    -- The embedded state serves to store the resulting set of
    -- transitions; the surface state serves to keep track of
    -- already visited nodes.
    doit i = do
        b <- E.gets $ S.member i
        E.when (not b) $ do
            E.modify $ S.insert i
            E.forM_ (D.edges i dawg) $ \(x, j) -> do
                E.lift . E.modify $ S.insert (i, x, j)
--                 E.lift . E.modify $ S.insert (i, j)
                doit j


-- | Compute the set of states given the set of transitions and
-- the root ID.  It works under the assumption that all states
-- are reachable from the start state.
states :: Ord a => a -> S.Set (a, b, a) -> S.Set a
states rootID edgeSet = S.fromList $ rootID : concat
    [[i, j] | (i, _, j) <- S.toList edgeSet]
