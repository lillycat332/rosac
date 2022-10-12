{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Rosalia.Language.Prelude where

import Data.Text qualified as T
import Data.Text.IO qualified as T

say :: T.Text -> IO ()
say = T.putStrLn

exampleMain :: IO ()
exampleMain = say "Hello, world!"