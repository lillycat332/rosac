
module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import Paths_Rosalia (version)
import Data.Version (showVersion)

data RunOptions = RunOptions
	{ optInput :: String }

data Command = Run RunOptions

data Options = Options
	{ optFile :: String
	, optOut :: String
--	, optCmd :: Command
	, optVerbose :: Bool
	, optWarn :: Bool
	}

options :: Parser Options
options = Options
	<$> strOption 
		( long "file"
		<> short 'f'
		<> metavar "FILE"
		<> help "File to process" )
	<*> strOption
		( long "output"
		<> short 'o'
		<> metavar "OUTPUT"
		<> showDefault
		<> value "a.out"
		<> help "Output file" )
	<*> switch
		( long "verbose"
		<> short 'v'
		<> help "Verbose output" )
	<*> switch
		( long "warn"
		<> short 'w'
		<> help "Show more warnings for potentially incorrect code" )

main = do
	opts <- execParser opts
	putStrLn ("rosac - the rosalia compiler. version: " <> (showVersion version))
	putStrLn ("compiling file: " <> optFile opts)
	putStrLn ("output: " <> optOut opts)
	putStrLn ("verbose: " <> show (optVerbose opts))
	putStrLn ("warn: " <> show (optWarn opts))
	where
		opts = info (options <**> helper)
			( fullDesc
			<> progDesc "Compile a rosalia file"
			<> header "rosalia - the reference compiler for the rosalia language" )
	

