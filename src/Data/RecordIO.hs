module Data.RecordIO where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                ( ByteString )
import           Data.Record

writeRecords :: Record b => FilePath -> [b] -> IO ()
writeRecords p recs =
  let constructors = constructor <$> recs
      concatted    = concatMap (++ "\n\n") constructors
  in  writeFile p concatted

parseRecords :: Record a => a -> ByteString -> Either String [a]
parseRecords src =
  let cruftParser = cruft src
  in  parseOnly (cruftParser *> sepBy (parser src) cruftParser)
