module Data.TypescriptInterface ( parseInterface
                                , Interface(..) ) where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.Parsers

import           Data.Casing
import           Data.Primitive
import           Data.Record                      (Record (..), getCasedFields)
import           Data.RecordIO                    (writeRecords)

data Interface = Interface { _fields :: [(String, Primitive)]
                           , _name   :: String } deriving (Eq, Show)

instance Record Interface where
  recordCasing = pure UpperCamel
  fieldCasing = pure Camel
  constructorKeyword = pure "interface"
  constructor = getConstructor
  typing = pure True
  fields = _fields
  name = _name
  fromRecord b = Interface (fields b) (name b)
  primitiveShow = pure $ tsPrinter
  parser = pure interfaceParser
  writer = pure writeRecords
  emptyR = Interface [] ""

getConstructor :: Interface -> String
getConstructor interface =
  let
    fieldMembers = getCasedFields interface ';' (Just "readonly")
  in
      "interface "
      ++ name interface
      ++ " {\n    "
      ++ fieldMembers
      ++ ";\n}"

fieldParser :: Parser (String, Primitive)
fieldParser = do
  try (string "readonly") *> skipSpace
  fieldName <- many' letterOrDigit <* char ':' <* skipSpace
  fieldType <- tsPrimParser <* skipSpace <* char ';'
  return (fieldName, fieldType)

interfaceParser :: Parser Interface
interfaceParser = do
  skipSpace
  interfaceName <- many' letterOrDigit <* skipSpace <* char '{' <* skipSpace
  interfaceFields <- skipSpace *> sepBy fieldParser skipSpace
  skipSpace <* char '}'
  return $ Interface interfaceFields interfaceName

parseInterface :: ByteString -> Either String Interface
parseInterface = parseOnly interfaceParser
