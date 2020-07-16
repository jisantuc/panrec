module RoundTripSpec
  ( roundTripSpec
  )
where

import           Data.Record
import qualified Data.Scala                    as Scala
import qualified Data.Typescript               as TS
import           Test.Hspec
import           Test.QuickCheck
import           Types

roundTripSpec :: Spec
roundTripSpec = do
  describe "Round trips" $ do
    it "should not change the input value for scala records" $ do
      property $ quickCheck unchangedScala
    it "should not change the input value for typescript records" $ do
      property $ quickCheck unchangedTypescript
    it "should roundtrip typescript -> scala -> typescript" $ do
      property $ quickCheck fromTypescriptRoundTrip
    it "should roundtrip scala -> typescript -> scala" $ do
      property $ quickCheck fromScalaRoundTrip

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
