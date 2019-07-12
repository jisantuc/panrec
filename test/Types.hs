module Types where

import           Data.Scala      (CaseClass (..))
import           Data.Typescript (Class (..))
import           Test.QuickCheck (Arbitrary (..))

newtype TestCaseClass = TestCaseClass CaseClass deriving (Show)
newtype TestTypescriptClass = TestTypescriptClass Class deriving (Show)

instance Arbitrary TestCaseClass where
  arbitrary = do
    fs <- arbitrary
    nm <- arbitrary
    return . TestCaseClass $ CaseClass fs nm

instance Arbitrary TestTypescriptClass where
  arbitrary = do
    fs <- arbitrary
    nm <- arbitrary
    return . TestTypescriptClass $ Class fs nm
