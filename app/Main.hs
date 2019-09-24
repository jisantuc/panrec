module Main where

import qualified Data.ByteString          as BS
import           Data.Record
import           Data.RecordIO
import qualified Data.Scala               as Scala
import           Data.Semigroup           ((<>))
import qualified Data.Typescript          as TS
import qualified Data.TypescriptInterface as TS
import           Options.Applicative
import           System.Exit              (ExitCode (..), exitWith)

data Options = Options { inLanguage  :: String
                       , outLanguage :: String
                       , inFile      :: String
                       , outFile     :: String } deriving Show

runPipeline :: (Record a, Record b) => a -> b -> BS.ByteString -> FilePath -> IO ()
runPipeline src dst input outf = do
  result <- pure $ parseRecords src input
  case result of
    Right recs ->
      (writer dst) outf (fromRecord <$> recs)
    Left err ->
      print err *> exitWith (ExitFailure 1)

options :: Parser Options
options = Options
  <$> strOption ( long "in-language"
                  <> short 'i'
                  <> metavar "INLANGUAGE"
                  <> help "Language of the input file" )
  <*> strOption ( long "out-language"
                  <> short 'o'
                  <> metavar "OUTLANGUAGE"
                  <> help "Language of the output file ")
  <*> argument str ( metavar "INPUTFILE" )
  <*> argument str ( metavar "OUTPUTFILE" )

run :: Options -> IO ()
run (Options inl outl inf outf ) =
  do
    inText <- BS.readFile inf
    case (inl, outl) of
      ("scala", "ts-class") ->
        runPipeline (emptyR :: Scala.CaseClass) (emptyR :: TS.Class) inText outf
      ("ts-class", "scala") ->
        runPipeline (emptyR :: TS.Class) (emptyR :: Scala.CaseClass) inText outf
      ("ts-class", "ts-interface") ->
        runPipeline (emptyR :: TS.Class) (emptyR :: TS.Interface) inText outf
      ("ts-interface", "ts-class") ->
        runPipeline (emptyR :: TS.Interface) (emptyR :: TS.Class) inText outf
      ("ts-interface", "scala") ->
        runPipeline (emptyR :: TS.Interface) (emptyR :: Scala.CaseClass) inText outf
      ("scala", "ts-interface") ->
        runPipeline (emptyR :: Scala.CaseClass) (emptyR :: TS.Interface) inText outf
      _ ->
        print "No idea what to do with those inputs" *> exitWith (ExitFailure 1)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "Convert from one language's record types to another's"
        <> header "panrec - like pandoc, but for record types" )
