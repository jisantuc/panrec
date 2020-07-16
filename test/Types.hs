module Types where

import           Data.Primitive                 ( Primitive(..) )
import           Data.Scala                     ( CaseClass(..) )
import           Data.Typescript                ( Class(..) )
import           Test.QuickCheck                ( Arbitrary(..) )
import           Test.QuickCheck.Gen            ( Gen
                                                , oneof
                                                )

newtype TestCaseClass =
  TestCaseClass CaseClass
  deriving (Show)

newtype TestTypescriptClass =
  TestTypescriptClass Class
  deriving (Show)

newtype TestPrimitive =
  TestPrimitive Primitive
  deriving (Show)

primGen :: Gen Primitive
primGen = oneof
  [ pure Int'
  , pure Double'
  , pure Float'
  , pure Char'
  , pure String'
  , pure Boolean'
  , do
    (TestPrimitive a) <- arbitrary
    return $ Option' a
  , do
    (TestPrimitive e) <- arbitrary
    (TestPrimitive a) <- arbitrary
    return $ Either' e a
  , do
    (TestPrimitive a) <- arbitrary
    return $ IO' a
  , do
    (TestPrimitive a) <- arbitrary
    return $ List' a
  , Vendor <$> arbitrary
  , pure Any
  ]

instance Arbitrary TestCaseClass where
  arbitrary = do
    fieldNames <- arbitrary
    types      <- traverse (pure primGen) fieldNames
    nm         <- arbitrary
    return . TestCaseClass $ CaseClass (fieldNames `zip` types) nm

instance Arbitrary TestTypescriptClass where
  arbitrary = do
    fieldNames <- arbitrary
    types      <- traverse (pure primGen) fieldNames
    nm         <- arbitrary
    return . TestTypescriptClass $ Class (fieldNames `zip` types) nm

instance Arbitrary TestPrimitive where
  arbitrary = TestPrimitive <$> primGen
