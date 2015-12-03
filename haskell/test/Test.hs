import           BasicPrelude
import qualified Test.Data.CRC24Q as CRC24Q
import           Test.Tasty

tests :: TestTree
tests =
  testGroup "Tests"
    [ CRC24Q.tests
    ]

main :: IO ()
main = defaultMain tests
