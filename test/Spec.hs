import           Data.Record
import           Test.Hspec
import           Test.QuickCheck
import           Types

main :: IO ()
main = hspec $ do
  describe "identity conversions" $
    it "should not change the input value" $
      property $ quickCheck unchangedScala

unchangedScala :: TestCaseClass -> Bool
unchangedScala (TestCaseClass x) = (fromRecord x) == x
