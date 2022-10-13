{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Semigroup ((<>))

import Control.Monad (when)
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_rosalia (version)
import Rosalia.Compiler.Backend.Log (crashAndBurn, logInfo, logVerbose)
import Rosalia.Compiler.Backend.Parser (programParser, runParser)

data Options = Options
  { optFile :: String,
    optOut :: String,
    -- 	, optCmd :: Command
    optVerbose :: Bool,
    optWarn :: Bool
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "File to process"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> showDefault
          <> value "a.out"
          <> help "Output file"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )
    <*> switch
      ( long "warn"
          <> short 'w'
          <> help "Show more warnings for potentially incorrect code"
      )

runOpts :: Options -> IO ()
runOpts (Options file output verbose warn) = do
  putStrLn ("\x1b[38;5;189mrosac - the rosalia compiler. version: " <> showVersion version <> "\x1b[0m")
  when verbose $ logVerbose "Verbose output enabled"

  fileC <- readFile file
  let fileC' = T.pack fileC
  when verbose $ logVerbose "Parsing..."
  let pTree = runParser programParser file fileC'
  when verbose $ do
    logVerbose "Parsing program"
  case pTree of
    Left err -> crashAndBurn $ T.pack $ show err
    Right tr' -> logInfo ("Parsed:\n  " <> T.pack (show tr'))

main :: IO ()
main = do
  runOpts =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Compile a rosalia file"
            <> header "rosalia - the reference compiler for the rosalia language"
        )
