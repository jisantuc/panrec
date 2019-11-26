module ScalaSpec
  ( scalaSpec
  ) where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Primitive
import           Data.Record
import           Data.RecordIO
import qualified Data.Scala            as Scala
import           Test.Hspec
import           Text.RawString.QQ

scalaSpec :: Spec
scalaSpec = do
  describe "Scala parsers" $ do
    it "should parse fields terminated with returns" $ do
      Scala.parseField exampleFieldReturn `shouldBe` (Right $ ("x", Int'))
    it "should parse fields terminated with spaces" $ do
      Scala.parseField exampleFieldSpace `shouldBe` (Right $ ("x", Int'))
    it "should parse fields not terminated by commas" $ do
      Scala.parseField exampleFieldLast `shouldBe` (Right $ ("x", Int'))
    it "should parse a field with an exotic higher-kinded type" $ do
      Scala.parseField fieldWithExoticHKT `shouldBe`
        (Right $ ("x", Option' $ VendorHK "Projected" [Vendor "Geometry"]))
    it "should parse a collection of fields" $ do
      Scala.parseFields justFields `shouldBe`
        (Right $ [("x", Int'), ("y", Int'), ("z", Int'), ("a", Int')])
    it "should read a scala case class correctly with spaces" $ do
      Scala.parseRecord exampleCaseClassSpaces `shouldBe`
        (Right $ Scala.CaseClass [("x", Int'), ("y", Int')] "Foo")
    it "should read a scala case class correctly with returns" $ do
      Scala.parseRecord exampleCaseClassReturns `shouldBe`
        (Right $ Scala.CaseClass [("x", Int'), ("y", String')] "Foo")
    it
      "should read a scala case class correctly with an exotic higher-kinded type" $ do
      Scala.parseRecord exampleCaseClassWithExoticHKT `shouldBe`
        (Right $
         Scala.CaseClass
           [("x", Option' $ VendorHK "Projected" [Vendor "Geometry"])]
           "Foo")
    it "should read a scala case class correctly with a default argument" $ do
      Scala.parseRecord exampleCaseClassWithDefaultArgs `shouldBe`
        (Right $
         Scala.CaseClass
           [ ("x", Option' $ VendorHK "Projected" [Vendor "Geometry"])
           , ("y", Option' Int')
           , ("z", Option' (Either' String' (Vendor "java.util.UUID")))
           ]
           "Foo")
    it "should parse several case classes when separated by cruft" $ do
      parseRecords (emptyR :: Scala.CaseClass) exampleSeveralCaseClasses `shouldBe`
        Right
          [ Scala.CaseClass [("x", Int'), ("y", Int')] "Foo"
          , Scala.CaseClass [("x", String')] "Bar"
          ]

exampleCaseClassReturns :: ByteString
exampleCaseClassReturns =
  pack
    [r| Foo(
  x: Int,
  y: String
)
|]

exampleCaseClassSpaces :: ByteString
exampleCaseClassSpaces = " Foo(x: Int, y: Int)\n"

exampleFieldReturn :: ByteString
exampleFieldReturn = "x: Int,\n"

exampleFieldSpace :: ByteString
exampleFieldSpace = "x: Int, "

exampleFieldLast :: ByteString
exampleFieldLast = "x: Int)"

justFields :: ByteString
justFields =
  [r|x: Int,
y: Int,
z: Int,
a: Int
)|]

fieldWithExoticHKT :: ByteString
fieldWithExoticHKT = "x: Option[Projected[Geometry]],"

exampleSeveralCaseClasses :: ByteString
exampleSeveralCaseClasses =
  [r|case class Foo(x: Int, y: Int)

object Foo {
  case class Bar(x: String)
}|]

exampleCaseClassWithExoticHKT :: ByteString
exampleCaseClassWithExoticHKT =
  [r| Foo(
  x: Option[Projected[Geometry]]
)
|]

exampleCaseClassWithDefaultArgs :: ByteString
exampleCaseClassWithDefaultArgs =
  [r| Foo(
  x: Option[Projected[Geometry]] = None,
  y: Option[Int] = Some(3),
  z: Option[Either[String, java.util.UUID]] = Some(Left("lol"))
)
|]
