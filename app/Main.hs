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
import           System.FilePath.Posix (dropTrailingPathSeparator,
                                        pathSeparator, takeBaseName)

data Options =
  Options
    { inLanguage  :: String
    , outLanguage :: String
    , inFile      :: String
    , outDir      :: String
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
  argument str (metavar "INPUTFILE") <*>
  argument str (metavar "OUTPUTDIR")

fileExt :: Map.Map String String
fileExt =
  Map.fromList
    [("ts-interface", ".ts"), ("ts-class", ".ts"), ("scala", ".scala")]

getOutFile :: FilePath -> FilePath -> String -> FilePath
getOutFile inf outDirectory lang =
  let newExt = fromMaybe ".unknown" (Map.lookup lang fileExt)
      base = takeBaseName inf
   in outDirectory ++ [pathSeparator] ++ base ++ newExt

runFile :: Options -> IO ()
runFile (Options inl outl inf outDirectory) =
  let outf = getOutFile inf outDirectory outl
   in case getPipeline inl outl of
        Right f  -> BS.readFile inf >>= (\x -> f x outf)
        Left err -> print err *> exitWith (ExitFailure 1)

run :: Options -> IO ()
run opts@(Options inl _ inf outDirectory) =
  let extension = fromMaybe ".unknown" (Map.lookup inl fileExt)
      getRelPath base f = base ++ [pathSeparator] ++ f
      cleanOutDir = dropTrailingPathSeparator outDirectory
      cleanInf = dropTrailingPathSeparator inf
   in do isRegularFile <- doesFileExist cleanInf
         if (not isRegularFile)
           then const () <$> do
                  dirContents <- listDirectory cleanInf
                  let absPaths = (getRelPath cleanInf) <$> dirContents
                  (\fp -> run (opts {inFile = fp, outDir = cleanOutDir})) `traverse`
                    absPaths
           else case (isSuffixOf extension cleanInf) of
                  True -> runFile opts {inFile = cleanInf, outDir = cleanOutDir}
                  False ->
                    print
                      ("Skipping " ++
                       cleanInf ++ " because it does not match " ++ extension)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Convert from one language's record types to another's" <>
         header "panrec - like pandoc, but for record types")
