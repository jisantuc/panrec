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
import           Data.Parsers
import           Data.Record                      (Casing (..), Record (..))

data CaseClass = CaseClass { _fields :: [String]
                           , _name   :: String } deriving (Eq, Show)

instance Record CaseClass where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructor _ = ("case class(" ++) . (++ ")")
  typing = pure True
  fields = _fields
  name = _name
  fromRecord b = CaseClass (fields b) (name b)


fieldParser :: Parser (String, String)
fieldParser = do
  _ <- skipWhile isSpace
  fieldName <- many' letterOrDigit
               <* try (many' space)
               <* char ':'
               <* try (many' space)
  fieldType <- many' letterOrDigit
               <* (try
                   (char ',' <|> endOfLineOrSpace)
                   <|> char ')')
  return (fieldName, fieldType)


caseClassParser :: Parser CaseClass
caseClassParser = do
  skipMany space
  _ <- "case class" <* skipSpace
  recordName <- many' letterOrDigit
  _ <- char '('
  skipSpace
  recordFields <- many' (fieldParser)
  skipSpace
  return $ CaseClass (fst <$> recordFields) recordName

parseField :: ByteString -> Either String (String, String)
parseField = parseOnly fieldParser

parseFields :: ByteString -> Either String [(String, String)]
parseFields = parseOnly (many' fieldParser)

parseRecord :: ByteString -> Either String CaseClass
parseRecord = parseOnly caseClassParser
