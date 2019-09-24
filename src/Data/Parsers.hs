module Data.Parsers where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Primitive

letterOrDigit :: Parser Char
letterOrDigit = letter_ascii <|> digit

endOfLineOrSpace :: Parser Char
endOfLineOrSpace = (pure ' ' <$> endOfLine) <|> space

tsPrimParser :: Parser Primitive
tsPrimParser =
  (\_ -> Double') <$> string "number"
  <|> (\_ -> String') <$> string "string"
  <|> (\_ -> Any) <$> string "any"
  <|> (\_ -> Boolean') <$> string "boolean"
  <|> List' <$> (string "Array<" *> tsPrimParser <* char '>')
  <|> Option' <$> (string "Option<" *> tsPrimParser <* char '>')
  <|> Either'
  <$> (string "Either<" *> tsPrimParser <* char ',' <* many' space)
  <*> (tsPrimParser <* char '>')
  <|> IO' <$> (string "Promise<" *> tsPrimParser <* char '>')
  <|> VendorHK
  <$> many' letterOrDigit <* char '<'
  <*> sepBy tsPrimParser (char ',' <* skipSpace)
  <* char '>'
  <|> Vendor <$> many' letterOrDigit
