module Data.Parsers where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8

letterOrDigit :: Parser Char
letterOrDigit = letter_ascii <|> digit

endOfLineOrSpace :: Parser Char
endOfLineOrSpace = (pure ' ' <$> endOfLine) <|> space
