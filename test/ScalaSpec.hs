module ScalaSpec (scalaSpec) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Primitive
import qualified Data.Scala            as Scala
import           Test.Hspec
import           Text.RawString.QQ

scalaSpec :: IO ()
scalaSpec = hspec $ do
  describe "Scala parsers" $ do
    it "should parse fields terminated with returns" $ do
      Scala.parseField exampleFieldReturn `shouldBe` (Right $ ("x", Int'))
    it "should parse fields terminated with spaces" $ do
      Scala.parseField exampleFieldSpace `shouldBe` (Right $ ("x", Int'))
    it "should parse fields not terminated by commas" $ do
      Scala.parseField exampleFieldLast `shouldBe` (Right $ ("x", Int'))
    it "should parse a collection of fields" $ do
      Scala.parseFields justFields `shouldBe` (Right $ [ ("x", Int')
                                                       , ("y", Int')
                                                       , ("z", Int')])
    it "should read a scala case class correctly with spaces" $ do
      Scala.parseRecord exampleCaseClassSpaces `shouldBe`
        (Right $ Scala.CaseClass [("x", Int'), ("y", Int')] "Foo")
--    it "should read a scala case class correctly with returns" $ do
--      Scala.parseRecord exampleCaseClassReturns `shouldBe`
--        (Right $ Scala.CaseClass [("x", Int'), ("y", String')] "Foo")

exampleCaseClassReturns :: ByteString
exampleCaseClassReturns = pack [r|case class Foo(
  x: Int,
  y: String
)\n|]

exampleCaseClassSpaces :: ByteString
exampleCaseClassSpaces = "case class Foo(x: Int, y: Int)\n"

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
