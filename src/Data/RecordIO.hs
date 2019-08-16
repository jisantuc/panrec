module Data.RecordIO where

import           Data.Record

writeRecords :: Record b => FilePath -> [b] -> IO ()
writeRecords p recs =
  let
    constructors = constructor <$> recs
    concatted = concatMap (++ "\n\n") constructors
  in
    writeFile p concatted
