module Data.Typescript ( parseField
                       , parseFunction
                       , parseBlock
                       , parseClass
                       , classParser
                       , Class(..) ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.Parsers
import           Data.Semigroup                   ((<>))

import           Data.Casing
import           Data.Primitive
import           Data.Record                      (Record (..), getCasedFields)
import           Data.RecordIO

data Class = Class { _fields :: [(String, Primitive)]
                   , _name   :: String } deriving (Eq, Show)

instance Record Class where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructorKeyword = pure "class"
  constructor = getConstructor
  typing = pure True
  fields = _fields
  name = _name
  fromRecord b = Class (fields b) (name b)
  primitiveShow = pure primitivePrinter
  parser = pure classParser
  writer = pure writeRecords
  emptyR = Class [] ""

getConstructor :: Class -> String
getConstructor cls =
  let
    fieldMembers = getCasedFields cls ';'
    constructorParams = getCasedFields cls ','
  in
    "class "
    ++ name cls
    ++ " {\n    "
    ++ fieldMembers
    ++ ";\n    constructor("
    ++ constructorParams
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
  t <- pure <$> const "" <$> char '}' <* skipSpace <|>
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
  <|> (\_ -> Any) <$> string "any"
  <|> (\_ -> Boolean') <$> string "boolean"
  <|> List' <$> (string "Array<" *> primParser <* char '>')
  <|> Option' <$> (string "Option<" *> primParser <* char '>')
  <|> Either'
  <$> (string "Either<" *> primParser <* char ',' <* many' space)
  <*> (primParser <* char '>')
  <|> IO' <$> (string "Promise<" *> primParser <* char '>')
  <|> Vendor <$> many' letterOrDigit

primitivePrinter :: Primitive -> String
primitivePrinter prim =
  case prim of
    Int' -> "number"
    Double' -> "number"
    Float' -> "number"
    Char' -> "string"
    String' -> "string"
    Boolean' -> "boolean"
    Any -> "any"
    Option' p -> "Option<" ++ primitivePrinter p ++ ">"
    Either' e a -> "Either<" ++ primitivePrinter e ++ ", " ++ primitivePrinter a ++ ">"
    IO' p -> "Promise<" ++ primitivePrinter p ++ ">"
    List' p -> "Array<" ++ primitivePrinter p ++ ">"
    Vendor s -> s

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
    *> pure ()

classParser :: Parser Class
classParser = do
  skipSpace
  className <- many' letterOrDigit <* skipSpace <* char '{' <* skipSpace
  classFields <- many' ( many' funcParser
                         *> skipSpace
                         *> fieldParser
                         <* skipSpace
                         <* many' funcParser )
                 <* skipSpace
  return $ Class classFields className

parseField :: ByteString -> Either String (String, Primitive)
parseField = parseOnly fieldParser

parseClass :: ByteString -> Either String Class
parseClass = parseOnly classParser

parseFunction :: ByteString -> Either String ()
parseFunction = parseOnly funcParser

parseBlock :: ByteString -> Either String ByteString
parseBlock = parseOnly blockParser
