import qualified Data.Scala as Scala
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Record
import           Test.Hspec
import           Test.QuickCheck
import           Text.RawString.QQ
import           Types

main :: IO ()
main = hspec $ do
  describe "identity conversions" $ do
    it "should not change the input value" $ do
      property $ quickCheck unchangedScala
  describe "Scala parsers" $ do
    it "should parse fields terminated with returns" $ do
      Scala.parseField exampleFieldReturn `shouldBe` (Right $ ("x", "Int"))
    it "should parse fields terminated with spaces" $ do
      Scala.parseField exampleFieldSpace `shouldBe` (Right $ ("x", "Int"))
    it "should parse fields not terminated by commas" $ do
      Scala.parseField exampleFieldLast `shouldBe` (Right $ ("x", "Int"))
    it "should parse a collection of fields" $ do
      Scala.parseFields justFields `shouldBe` (Right $ [ ("x", "Int")
                                                       , ("y", "Int")
                                                       , ("z", "Int")])
    it "should read a scala case class correctly with spaces" $ do
      Scala.parseRecord exampleCaseClassSpaces `shouldBe` (Right $ Scala.CaseClass ["x", "y"] "Foo")
    it "should read a scala case class correctly with returns" $ do
      Scala.parseRecord exampleCaseClassReturns `shouldBe` (Right $ Scala.CaseClass ["x", "y"] "Foo")


unchangedScala :: TestCaseClass -> Bool
unchangedScala (TestCaseClass x) = (fromRecord x) == x

exampleCaseClassReturns :: ByteString
exampleCaseClassReturns = pack [r|case class Foo(
  x: Int,
  y: String
)|]

exampleCaseClassSpaces :: ByteString
exampleCaseClassSpaces = "case class Foo(x: Int, y: Int)"

exampleFieldReturn :: ByteString
exampleFieldReturn = "x: Int,\n"

exampleFieldSpace :: ByteString
exampleFieldSpace = "x: Int, "

exampleFieldLast :: ByteString
exampleFieldLast = "x: Int)"

justFields :: ByteString
justFields = pack [r|x: Int,
y: Int,
z: Int,
a: Int|]
