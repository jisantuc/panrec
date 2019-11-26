module Data.Casing
  ( Casing(..)
  , reCase
  , joinParts
  , splitParts
  ) where

import           Data.Char (isAsciiUpper, toLower, toUpper)
import           Data.List (intersperse)

data Casing
  = Camel
  | UpperCamel
  | Snake
  | UpperSnake
  | Upper
  | Lower
  deriving (Eq, Show)

-- | With a particular casing style, split a single string into
-- its component parts
splitParts :: Casing -> String -> [String]
splitParts _ "" = []
splitParts casing s =
  case casing of
    Camel      -> go isAsciiUpper s "" [] id
    UpperCamel -> go isAsciiUpper s "" [] id
    Snake      -> go (== '_') s "" [] (filter (/= '_'))
    UpperSnake -> go (== '_') s "" [] (filter (/= '_'))
    Upper      -> [toLower <$> s]
    Lower      -> [toLower <$> s]
  where
    go _ "" stringAccum partsAccum after =
      after <$> (partsAccum ++ pure stringAccum)
    go predicate (ch:chs) "" partsAccum after =
      go predicate chs (pure . toLower $ ch) partsAccum after
    go predicate (ch:chs) stringAccum partsAccum after =
      if (not $ predicate ch)
        then go
               predicate
               chs
               (stringAccum ++ (pure . toLower $ ch))
               partsAccum
               after
        else go
               predicate
               chs
               (pure . toLower $ ch)
               (partsAccum ++ pure stringAccum)
               after

-- | With a particular casing style, create a single string from
-- the component parts of a string
joinParts :: Casing -> [String] -> String
joinParts _ [] = ""
joinParts casing vals@(x:xs) =
  case casing of
    Camel      -> mconcat (x : (toTitle <$> xs))
    UpperCamel -> mconcat $ toTitle <$> vals
    Snake      -> mconcat $ intersperse "_" vals
    UpperSnake -> mconcat $ intersperse "_" (toTitle <$> vals)
    Upper      -> mconcat $ (toUpper <$>) <$> vals
    Lower      -> mconcat $ (toLower <$>) <$> vals

-- | Translate a string in one casing into a string in another
reCase :: Casing -> Casing -> String -> String
reCase casingIn casingOut = joinParts casingOut . splitParts casingIn

toTitle :: String -> String
toTitle ""     = ""
toTitle (x:xs) = toUpper x : xs
