module Data.Scala (
  CaseClass(..)
  , caseClassParser
  , fieldParser
  , parseField
  , parseFields
  , parseRecord) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.Casing
import           Data.Parsers
import           Data.Primitive
import           Data.Record                      (Record (..), getCasedFields)
import           Data.RecordIO

data CaseClass = CaseClass { _fields :: [(String, Primitive)]
                           , _name   :: String } deriving (Eq, Show)

instance Record CaseClass where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructorKeyword = pure "case class"
  constructor = getConstructor
  typing = pure True
  fields = _fields
  name = _name
  fromRecord b = CaseClass (fields b) (name b)
  primitiveShow = pure primitivePrinter
  parser = pure caseClassParser
  writer = pure writeRecords
  emptyR = CaseClass [] ""

getConstructor :: Record b => b -> String
getConstructor record =
  "case class " ++ name record ++ "("  ++ getCasedFields record ',' Nothing ++ ")"

fieldParser :: Parser (String, Primitive)
fieldParser = do
  _ <- skipWhile isSpace
  fieldName <- many' letterOrDigit
               <* try (many' space)
               <* char ':'
               <* try (many' space)
  fieldType <- primParser
               <* ( char ','
                    <|> (many' endOfLineOrSpace *> char ')')
                  )
  return (fieldName, fieldType)

primParser :: Parser Primitive
primParser =
  (\_ -> Int') <$> string "Int"
  <|> (\_ -> Double') <$> string "Double"
  <|> (\_ -> Float') <$> string "Float"
  <|> (\_ -> Char') <$> string "Char"
  <|> (\_ -> String') <$> string "String"
  <|> (\_ -> Boolean') <$> string "Boolean"
  <|> (\_ -> Any) <$> string "Any"
  <|> Option' <$> (string "Option[" *> primParser <* char ']')
  <|> Either'
  <$> (string "Either[" *> primParser <* char ',' <* many' space)
  <*> (primParser <* char ']')
  <|> IO' <$> (string "IO[" *> primParser <* char ']')
  <|> List' <$> (string "List[" *> primParser <* char ']')
  <|> VendorHK <$>
  (many' letterOrDigit) <* char '['
  <*> sepBy primParser (char ',' <* skipSpace)
  <* char ']'
  <|> Vendor <$> many' letterOrDigit

primitivePrinter :: Primitive -> String
primitivePrinter = printer "[" "]"

caseClassParser :: Parser CaseClass
caseClassParser = do
  recordName <- skipSpace *> many' letterOrDigit
  _ <- char '('
  skipSpace
  recordFields <- many' fieldParser
  endOfLine
  return $ CaseClass recordFields recordName

parseField :: ByteString -> Either String (String, Primitive)
parseField = parseOnly fieldParser

parseFields :: ByteString -> Either String [(String, Primitive)]
parseFields = parseOnly (many' fieldParser)

parseRecord :: ByteString -> Either String CaseClass
parseRecord = parseOnly caseClassParser
