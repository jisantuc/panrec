import qualified Data.Casing    as Casing
import           RoundTripSpec  (roundTripSpec)
import           ScalaSpec      (scalaSpec)
import           Test.Hspec
import           TypescriptSpec (typescriptSpec)

main :: IO ()
main = do
  scalaSpec
  typescriptSpec
  roundTripSpec
  hspec $ do
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
