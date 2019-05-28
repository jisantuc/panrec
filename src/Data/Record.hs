module Data.Record where

data Casing = Camel
  | UpperCamel
  | Snake
  | UpperSnake
  | Upper
  | Lower
  deriving (Eq, Show)

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
  constructor :: a -> String -> String
  -- | Whether this record type includes type information
  typing :: a -> Bool
  -- | Records must provide a way to access their fields
  fields :: a -> [String]
  -- | Build this type from another record
  fromRecord :: Record b => b -> a

-- | With a particular casing style, create a single string from
-- the component parts of a string
caseParts :: Casing -> [String] -> String
caseParts = undefined

-- | With a particular casing style, split a single string into
-- its component parts
splitStringParts :: Casing -> String -> [String]
splitStringParts = undefined

-- | Translate a string in one casing into a string in another
reCase :: Casing -> Casing -> String -> String
reCase casingIn casingOut = caseParts casingOut . splitStringParts casingIn
