module TypescriptSpec
  ( typescriptSpec
  ) where

import           Data.Attoparsec.ByteString.Char8 (match, parseOnly)
import           Data.ByteString                  (ByteString)
import           Data.Primitive
import           Data.Record
import           Data.RecordIO
import qualified Data.Typescript                  as TS
import           Test.Hspec
import           Text.RawString.QQ

typescriptSpec :: Spec
typescriptSpec = do
  describe "Typescript parsers" $ do
    it "should parse a field correctly" $ do
      TS.parseField exampleTypescriptField `shouldBe`
        (Right $ ("greeting", String'))
    it "should parse a field correctly with an exotic higher-kinded type" $ do
      TS.parseField exampleTypescriptFieldExoticHKT `shouldBe`
        (Right $ ("friends", VendorHK "ReadonlyArray" [String']))
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
        (Right $
         TS.Class [("greeting", String'), ("otherField", Double')] "Greeter")
    it "should parse a whole class correctly with functions" $ do
      TS.parseClass exampleTypescriptClass `shouldBe`
        (Right $
         TS.Class [("greeting", String'), ("otherField", Double')] "Greeter")
    it "should parse several classes" $ do
      parseRecords (emptyR :: TS.Class) exampleSeveralClasses `shouldBe`
        (Right
           [ TS.Class [("x", Double'), ("y", String')] "Foo"
           , TS.Class [("x", String')] "Bar"
           ])
    it "should parse a class after cruft" $ do
      parseRecords (emptyR :: TS.Class) exampleClassAfterCruft `shouldBe`
        (Right
           [TS.Class [("greeting", String'), ("otherField", Double')] "Greeter"])
    it "should parse a class before cruft" $ do
      parseRecords (emptyR :: TS.Class) exampleClassBeforeCruft `shouldBe`
        (Right
           [TS.Class [("greeting", String'), ("otherField", Double')] "Greeter"])
    it "should parse all the cruft" $ do
      parseOnly (match $ cruft (emptyR :: TS.Class)) exampleCruft `shouldBe`
        (Right (exampleCruft, ()))
    it "should not parse a class as cruft" $ do
      parseOnly
        (match $ cruft (emptyR :: TS.Class))
        exampleTypescriptClassNoFunc `shouldBe`
        (Left "c: not enough input")
    it "should be mad if the cruft never ends" $ do
      parseOnly (match $ cruft (emptyR :: TS.Class)) exampleCruftNoTermination `shouldBe`
        (Left "c: not enough input")

exampleTypescriptField :: ByteString
exampleTypescriptField = "    greeting: string;"

exampleTypescriptFieldExoticHKT :: ByteString
exampleTypescriptFieldExoticHKT = "    friends: ReadonlyArray<string>;"

exampleTypescriptClassNoFunc :: ByteString
exampleTypescriptClassNoFunc =
  [r| Greeter {
    greeting: string;
    otherField: number;
}
|]

exampleTypescriptClass :: ByteString
exampleTypescriptClass =
  [r| Greeter {
    greeting: string;
    constructor(message: string, value: int) {
        this.greeting = message;
        this.otherField = value;
    }
    greet() {
        return "Hello, " + this.greeting;
    }
    otherField: number;
}
|]

exampleClassFunction :: ByteString
exampleClassFunction =
  [r|constructor(message: string) {
    this.greeting = message;
}|]

exampleNormalFunction :: ByteString
exampleNormalFunction =
  [r|
  greet(who: string) {
    return "Hello, " + who;
  }|]

exampleBlock :: ByteString
exampleBlock = "{lol}"

exampleBlockWithNesting :: ByteString
exampleBlockWithNesting =
  [r|{
  if (x == 3) {
    return false;
  } else {
    return true;
  }
}|]

exampleClassAfterCruft :: ByteString
exampleClassAfterCruft =
  [r|
some garbage
class Greeter {
    greeting: string;
    otherField: number;
}
|]

exampleClassBeforeCruft :: ByteString
exampleClassBeforeCruft =
  [r|class Greeter {
    greeting: string;
    otherField: number;
}

some other garbage
|]

exampleSeveralClasses :: ByteString
exampleSeveralClasses =
  [r|class Foo {
    x: number;
    y: string;
    constructor(x: number, y: string) {
        this.x = x;
        this.y = y;
    }
}

class Bar {
    x: string;
    constructor(x: string) {
        this.x = x;
    }
}
|]

exampleCruft :: ByteString
exampleCruft =
  [r|
sure

whatever

great

class|]

exampleCruftNoTermination :: ByteString
exampleCruftNoTermination =
  [r|
sure

whatever

great

keyword

never

gonna

happen|]
