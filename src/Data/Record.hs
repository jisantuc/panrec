module Data.Record where

import           Data.Attoparsec.ByteString.Char8
import           Data.Casing
import           Data.List                        (intersperse)
import           Data.Primitive

-- | Record is a generic container for things that have constructors,
-- have fields, and know things about the style patterns of the language
-- that they're written in.
class Record a where
  -- | How the record type constructor should be cased, e.g.,
  -- UpperCamel would indicate
  -- data FooBar
  recordCasing :: a -> Casing
  -- | How fields in this record should be cased, e.g.,
  -- Camel would indicate
  -- data Foo = Foo { fooBar :: Int, ... },
  -- while Snake would indicate
  -- data Foo = Foo { foo_bar :: Int, ... }
  fieldCasing :: a -> Casing
  -- | The keyword and syntax around this record type, e.g.,
  -- data Foo = Foo { bar :: Int, ... }
  constructor :: a -> String
  -- | Whether this record type includes type information
  typing :: a -> Bool
  -- | Records must provide a way to access their fields
  fields :: a -> [(String, Primitive)]
  -- | Records must provide a way to access their names
  name :: a -> String
  -- | Build this type from another record
  fromRecord :: Record b => b -> a
  -- | Print primitives as types native to this language
  primitiveShow :: a -> Primitive -> String
  -- | Carry around information about how to parse this record from source
  parser :: a -> Parser a
  -- | Carry around information about how to write this record to source
  writer :: a -> FilePath -> [a] -> IO ()
  emptyR :: a

-- | Get fields for this record type with their types
-- Currently this assumes that you want Scala, Python3, or TypeScript-style
-- type annotations (single :, a space, and the type name). That will have to
-- change to support more languages
getCasedFields :: Record b => b -> Char -> String
getCasedFields record sep =
  let
    caser field =
      reCase (fieldCasing record) Camel (fst field)
      ++ ": "
      ++ primitiveShow record (snd field)
  in
    mconcat . intersperse (sep : "\n") $ caser <$> (fields record)
