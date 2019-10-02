module Pipeline
  ( getPipeline
  ) where

import qualified Data.ByteString          as BS
import           Data.Record
import           Data.RecordIO
import qualified Data.Scala               as Sc
import qualified Data.Typescript          as TS
import qualified Data.TypescriptInterface as TS
import           System.Exit              (ExitCode (..), exitWith)

type Pipeline = BS.ByteString -> FilePath -> IO ()

runPipeline ::
     forall a b. (Record a, Record b)
  => BS.ByteString
  -> FilePath
  -> IO ()
runPipeline input outf = do
  result <- pure $ parseRecords (emptyR @a) input
  case result of
    Right recs -> (writer (emptyR @b)) outf (fromRecord <$> recs)
    Left err   -> print err *> exitWith (ExitFailure 1)

getPipeline :: String -> String -> Either String Pipeline
getPipeline s1 s2 =
  case (s1, s2) of
    ("scala", "ts-class") -> Right $ runPipeline @Sc.CaseClass @TS.Class
    ("ts-class", "scala") -> Right $ runPipeline @TS.Class @Sc.CaseClass
    ("ts-class", "ts-interface") -> Right $ runPipeline @TS.Class @TS.Interface
    ("ts-interface", "ts-class") -> Right $ runPipeline @TS.Interface @TS.Class
    ("ts-interface", "scala") -> Right $ runPipeline @TS.Interface @Sc.CaseClass
    ("scala", "ts-interface") -> Right $ runPipeline @Sc.CaseClass @TS.Interface
    _ -> Left $ "Cannot build pipeline with " ++ s1 ++ " and " ++ s2
