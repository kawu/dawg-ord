import           Test.Tasty (defaultMain, testGroup)

import qualified Data.DAWG.Ord.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Data.DAWG.Ord.Tests.tests
    ]
