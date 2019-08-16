module TypescriptSpec (typescriptSpec) where

import           Data.ByteString   (ByteString)
import           Data.Primitive
import qualified Data.Typescript   as TS
import           Test.Hspec
import           Text.RawString.QQ

typescriptSpec :: IO ()
typescriptSpec = hspec $ do
  describe "Typescript parsers" $ do
    it "should parse a field correctly" $ do
      TS.parseField exampleTypescriptField `shouldBe` (Right $ ("greeting", String'))
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
        (Right $ TS.Class [("greeting", String'), ("otherField", Double')] "Greeter")
    it "should parse a whole class correctly with functions" $ do
      TS.parseClass exampleTypescriptClass `shouldBe`
        (Right $ TS.Class [("greeting", String'), ("otherField", Double')] "Greeter")

exampleTypescriptField :: ByteString
exampleTypescriptField =
  "    greeting: string;"


exampleTypescriptClassNoFunc ::  ByteString
exampleTypescriptClassNoFunc = [r|class Greeter {
    greeting: string;
    otherField: number;
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
    otherField: number;
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
