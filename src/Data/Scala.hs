module Data.Scala where

import           Data.Record (Casing (..), Record (..))

data CaseClass = CaseClass { _fields :: [String] } deriving (Eq, Show)

instance Record CaseClass where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructor _ = ("case class(" ++) . (++ ")")
  typing = pure True
  fields = _fields
  fromRecord b = CaseClass $ fields b
