{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Rosalia.Compiler.Frontend.Log where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Exit (exitFailure)
import System.IO (stderr, stdout)

eOut :: T.Text -> IO ()
eOut = T.hPutStrLn stdout

-- | Log an error message. This is to be used for errors that are not
--   necessarily fatal, but should be reported to the user, such as syntax errors
--   or type errors in the provided code.
logErr :: T.Text -> IO ()
logErr msg = eOut ("\x1b[1;31m[ERROR]: " <> msg <> "\x1b[0m")

-- | Log a warning message.
logWarn :: T.Text -> IO ()
logWarn msg = eOut ("\x1b[1;33m[WARN]: " <> msg <> "\x1b[0m")

-- | Log an info message.
logInfo :: T.Text -> IO ()
logInfo msg = eOut ("\x1b[1;36m[info]: " <> msg <> "\x1b[0m")

-- | Log a message intended only for verbose output.
logVerbose :: T.Text -> IO ()
logVerbose msg = eOut ("\x1b[1;36m[info (verbose)]: " <> msg <> "\x1b[0m")

-- | Print a panic message, instructing the user to report a bug. logPanic on it's own
--   does not exit the program, so it is recommended to use 'crashAndBurn'.
logPanic :: T.Text -> IO ()
logPanic msg =
  eOut
    ( "\x1b[1;31mpanic: "
        <> msg
        <> "\x1b[133m\n"
        <> "\n  The above fatal error was encountered while trying to compile your program."
        <> "\n  If you know how to reproduce this error, please file a bug report at:\n"
        <> "\n  https://github.com/lillycat332/rosac/issues\n"
        <> "\n  Please include the source code that caused this error."
        <> "\n  It is likely that this is a bug in the compiler.\x1b[0m"
    )

-- | Panic and die. This is a fatal error. The compiler will exit with a non-zero
--   exit code.
crashAndBurn :: T.Text -> IO ()
crashAndBurn msg = do
  logPanic msg
  exitFailure
