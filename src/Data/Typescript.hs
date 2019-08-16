module Data.Typescript ( parseField
                       , parseFunction
                       , parseBlock
                       , parseClass
                       , Class(..) ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.Parsers

import           Data.Casing
import           Data.Primitive
import           Data.Record                      (Record (..), getCasedFields)

data Class = Class { _fields :: [(String, Primitive)]
                   , _name   :: String } deriving (Eq, Show)

instance Record Class where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructor = getConstructor
  typing = pure True
  fields = _fields
  name = _name
  fromRecord b = Class (fields b) (name b)

getConstructor :: Class -> String
getConstructor cls =
  let
    casedFields = getCasedFields cls
  in
    "class "
    ++ name cls
    ++ "{\n    constructor("
    ++ casedFields
    ++ ") {\n"
    ++ getPropAssignments cls
    ++ "    }\n}"

getPropAssignments :: Class -> String
getPropAssignments cls =
  let
    mkAssignment :: (String, Primitive) -> String
    mkAssignment fld =
      "        this." ++ fst fld ++ " = " ++ fst fld ++ ";"
  in
    concatMap (++ "\n") $ mkAssignment <$> fields cls

-- | Parse blocks between pairs of {}, discarding what's in the middle
blockParser :: Parser ByteString
blockParser = do
  skipSpace
  _ <- many' letterOrDigit <* skipSpace
  char '{' *> skipSpace
  h <- takeTill (\x -> x == '{' || x == '}')
  t <- pure <$> const "" <$> string "}" <* skipSpace <|>
    many1 blockParser <* skipSpace
  pure $ h <> mconcat t

fieldParser :: Parser (String, Primitive)
fieldParser = do
  skipMany space
  fieldName <- many' letterOrDigit <* skipSpace <* char ':'
  fieldType <- skipSpace *> primParser <* skipSpace <* char ';'
  return (fieldName, fieldType)

primParser :: Parser Primitive
primParser =
  (\_ -> Double') <$> string "number"
  <|> (\_ -> String') <$> string "string"
  <|> List' <$> (string "Array<" *> primParser <* char '>')
  <|> (\_ -> Any) <$> string "any"
  <|> (\_ -> Boolean') <$> string "boolean"
  <|> Vendor <$> many' letterOrDigit

funcArgParser :: Parser (String, String)
funcArgParser =  do
  skipMany space
  argName <- many' letterOrDigit <* skipSpace <* char ':'
  argType <- skipSpace *> many' letterOrDigit <* skipSpace
  return (argName, argType)

funcParser :: Parser ()
funcParser = do
  skipSpace
  many' letterOrDigit
    *> char '('
    *> sepBy funcArgParser (char ',')
    *> char ')'
    *> blockParser
    *> skipSpace

classParser :: Parser Class
classParser = do
  className <- "class " *> many' letterOrDigit <* skipSpace <* char '{' <* skipSpace
  classFields <- many' (many' funcParser *> fieldParser <* skipSpace <* many' funcParser)
  return $ Class (classFields) className

parseField :: ByteString -> Either String (String, Primitive)
parseField = parseOnly fieldParser

parseClass :: ByteString -> Either String Class
parseClass = parseOnly classParser

parseFunction :: ByteString -> Either String ()
parseFunction = parseOnly funcParser

parseBlock :: ByteString -> Either String ByteString
parseBlock = parseOnly blockParser
