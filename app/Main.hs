module Main where

import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Pipeline            (getPipeline)
import           System.Exit         (ExitCode (..), exitWith)

data Options =
  Options
    { inLanguage  :: String
    , outLanguage :: String
    , inFile      :: String
    }
  deriving (Show)

options :: Parser Options
options =
  Options <$>
  strOption
    (long "in-language" <> short 'i' <> metavar "INLANGUAGE" <>
     help "Language of the input file") <*>
  strOption
    (long "out-language" <> short 'o' <> metavar "OUTLANGUAGE" <>
     help "Language of the output file ") <*>
  argument str (metavar "INPUTFILE")

fileExt :: Map.Map String String
fileExt =
  Map.fromList
    [("ts-interface", ".ts"), ("ts-class", ".ts"), ("scala", ".scala")]

getOutFile :: FilePath -> String -> FilePath
getOutFile inf lang =
  takeWhile (/= '.') inf ++ fromMaybe ".unknown" (Map.lookup lang fileExt)

run :: Options -> IO ()
run (Options inl outl inf) =
  let outf = getOutFile inf outl
   in case getPipeline inl outl of
        Right f  -> BS.readFile inf >>= (\x -> f x outf)
        Left err -> print err *> exitWith (ExitFailure 1)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Convert from one language's record types to another's" <>
         header "panrec - like pandoc, but for record types")
