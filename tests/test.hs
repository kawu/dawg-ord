import           Test.Tasty (defaultMain, testGroup)

import qualified Ord


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Ord.tests
    ]
