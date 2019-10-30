module Main where

import qualified Data.ByteString       as BS
import           Data.List             (isSuffixOf)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        ((<>))
import           Options.Applicative
import           Pipeline              (getPipeline)
import           System.Directory
import           System.Exit           (ExitCode (..), exitWith)
import           System.FilePath.Posix (pathSeparator, splitExtension)

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
  let newExt = fromMaybe ".unknown" (Map.lookup lang fileExt)
      (base, _) = splitExtension inf
   in base ++ newExt

runFile :: Options -> IO ()
runFile (Options inl outl inf) =
  let outf = getOutFile inf outl
   in case getPipeline inl outl of
        Right f  -> BS.readFile inf >>= (\x -> f x outf)
        Left err -> print err *> exitWith (ExitFailure 1)

run :: Options -> IO ()
run opts@(Options inl _ inf) =
  let extension = fromMaybe ".unknown" (Map.lookup inl fileExt)
      getRelPath base f = base ++ [pathSeparator] ++ f
   in do isRegularFile <- doesFileExist inf
         if (not isRegularFile)
           then const () <$> do
                  dirContents <- listDirectory inf
                  let absPaths = (getRelPath inf) <$> dirContents
                  (\fp -> run (opts {inFile = fp})) `traverse` absPaths
           else case (isSuffixOf extension inf) of
                  True -> runFile opts
                  False ->
                    print
                      ("Skipping " ++
                       inf ++ " because it does not match " ++ extension)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Convert from one language's record types to another's" <>
         header "panrec - like pandoc, but for record types")
