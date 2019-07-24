import qualified Data.Casing as Casing
import qualified Data.Scala as Scala
import qualified Data.Typescript as TS
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Record
import           Test.Hspec
import           Test.QuickCheck
import           Text.RawString.QQ
import           Types

main :: IO ()
main = hspec $ do
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
  describe "Typescript parsers" $ do
    it "should parse a field correctly" $ do
      TS.parseField exampleTypescriptField `shouldBe` (Right $ ("greeting", "string"))
    it "should parse a constructor function correctly" $ do
      TS.parseFunction exampleClassFunction `shouldBe` (Right ())
    it "should parse a normal function correctly" $ do
      TS.parseFunction exampleNormalFunction `shouldBe` (Right ())
    it "should parse unnested blocks correctly" $ do
      TS.parseBlock exampleBlock `shouldBe` (Right "lol")
    it "should parse nested blocks correctly" $ do
      TS.parseBlock exampleBlockWithNesting `shouldBe`
        (Right "if (x == 3) return false;\n  return true;\n  ")
    it "should parse a whole class correctly without functions" $ do
      TS.parseClass exampleTypescriptClassNoFunc `shouldBe`
        (Right $ TS.Class ["greeting", "otherField"] "Greeter")
    it "should parse a whole class correctly with functions" $ do
      TS.parseClass exampleTypescriptClass `shouldBe`
        (Right $ TS.Class ["greeting", "otherField"] "Greeter")
  describe "Round trips" $ do
    it "should not change the input value for scala records" $ do
      property $ quickCheck unchangedScala
    it "should not change the input value for typescript records" $ do
      property $ quickCheck unchangedTypescript
    it "should roundtrip typescript -> scala -> typescript" $ do
      property $ quickCheck fromTypescriptRoundTrip
    it "should roundtrip scala -> typescript -> scala" $ do
      property $ quickCheck fromScalaRoundTrip

  describe "Splitting with casings" $ do
    it "should split a camel-case string" $ do
      Casing.splitParts Casing.Camel "aGoodString" `shouldBe` ["a", "good", "string"]
    it "should split an upper camel-case string" $ do
      Casing.splitParts Casing.UpperCamel "AVeryGoodString" `shouldBe` ["a", "very", "good", "string"]
    it "should split a snake case string" $ do
      Casing.splitParts Casing.Snake "what_a_nice_string" `shouldBe` ["what", "a", "nice", "string"]
    it "should split an upper snake case string" $ do
      Casing.splitParts Casing.UpperSnake "What_A_Nice_String" `shouldBe` ["what", "a", "nice", "string"]
    it "should be understandably baffled by all upper-case string" $ do
      Casing.splitParts Casing.Upper "WHATANICESTRING" `shouldBe` ["whatanicestring"]
    it "should be understandably baffled by all lower-case string" $ do
      Casing.splitParts Casing.Lower "whatanicestring" `shouldBe` ["whatanicestring"]
  describe "Joining with casings" $ do
    it "should join a camel-case string" $ do
      Casing.joinParts Casing.Camel  ["a", "good", "string"] `shouldBe` "aGoodString"
    it "should join an upper camel-case string" $ do
      Casing.joinParts Casing.UpperCamel  ["a", "very", "good", "string"] `shouldBe` "AVeryGoodString"
    it "should join a snake case string" $ do
      Casing.joinParts Casing.Snake  ["what", "a", "nice", "string"] `shouldBe` "what_a_nice_string"
    it "should join an upper snake case string" $ do
      Casing.joinParts Casing.UpperSnake  ["what", "a", "nice", "string"] `shouldBe` "What_A_Nice_String"
    it "should be understandably baffled by all upper-case string" $ do
      Casing.joinParts Casing.Upper  ["whatanicestring"] `shouldBe` "WHATANICESTRING"
    it "should be understandably baffled by all lower-case string" $ do
      Casing.joinParts Casing.Lower  ["whatanicestring"] `shouldBe` "whatanicestring"

unchangedScala :: TestCaseClass -> Bool
unchangedScala (TestCaseClass x) = (fromRecord x) == x

unchangedTypescript :: TestTypescriptClass -> Bool
unchangedTypescript (TestTypescriptClass x) = (fromRecord x) == x

fromTypescriptRoundTrip :: TestTypescriptClass -> Bool
fromTypescriptRoundTrip (TestTypescriptClass x) =
  (fromRecord . (fromRecord :: TS.Class -> Scala.CaseClass) $ x) == x

fromScalaRoundTrip :: TestCaseClass -> Bool
fromScalaRoundTrip (TestCaseClass x) =
  (fromRecord . (fromRecord :: Scala.CaseClass -> TS.Class) $ x) == x

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

exampleTypescriptField :: ByteString
exampleTypescriptField =
  "    greeting: string;"


exampleTypescriptClassNoFunc ::  ByteString
exampleTypescriptClassNoFunc = [r|class Greeter {
    greeting: string;
    otherField: int;
}|]

exampleTypescriptClass :: ByteString
exampleTypescriptClass = [r|class Greeter {
    greeting: string;
    constructor(message: string, value: int) {
        this.greeting = message;
        this.otherField = value;
    }
    greet() {
        return "Hello, " + this.greeting;
    }
    otherField: int;
}|]

exampleClassFunction :: ByteString
exampleClassFunction = [r|constructor(message: string) {
    this.greeting = message;
}|]

exampleNormalFunction :: ByteString
exampleNormalFunction = [r|
  greet(who: string) {
    return "Hello, " + who;
  }|]

exampleBlock :: ByteString
exampleBlock = "{lol}"

exampleBlockWithNesting :: ByteString
exampleBlockWithNesting = [r|{
  if (x == 3) {
    return false;
  } else {
    return true;
  }
}|]
