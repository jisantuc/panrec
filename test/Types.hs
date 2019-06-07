module Types where

import           Data.Scala      (CaseClass (..))
import           Test.QuickCheck

newtype TestCaseClass = TestCaseClass CaseClass deriving (Show)

instance Arbitrary TestCaseClass where
  arbitrary = do
    fs <- arbitrary
    nm <- arbitrary
    return . TestCaseClass $ CaseClass fs nm
