import           BasicPrelude
import qualified Test.Data.CRC24Q       as CRC24Q
import qualified Test.Data.RTCM3.Extras as Extras
import qualified Test.Data.RTCM3.System as System
import           Test.Tasty

tests :: TestTree
tests =
  testGroup "Tests"
    [ CRC24Q.tests
    , Extras.tests
    , System.tests
    ]

main :: IO ()
main = defaultMain tests
