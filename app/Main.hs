{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Semigroup ((<>))
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_Rosalia (version)
import Rosalia.Log

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

main :: IO ()
main = do
  popts <- execParser opts
  file <- readFile (optFile popts)

  putStrLn ("\x1b[38;5;189mrosac - the rosalia compiler. version: " <> showVersion version <> "\x1b[0m")
  logErr "test error"
  logWarn "test warning"

  logInfo ("entering file: " <> T.pack (optFile popts))

  crashAndBurn "HOW'S THIS FOR AN ERROR? HUH?"
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Compile a rosalia file"
            <> header "rosalia - the reference compiler for the rosalia language"
        )
