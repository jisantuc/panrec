module Main where

import qualified Data.ByteString     as BS
import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options { inLanguage  :: String
                       , outLanguage :: String
                       , inFile      :: String
                       , outFile     :: String } deriving Show

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
run (Options _ _ inf outf ) = do
  inText <- BS.readFile inf
  BS.writeFile outf inText

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "Convert from one language's record types to another's"
        <> header "panrec - like pandoc, but for record types" )
