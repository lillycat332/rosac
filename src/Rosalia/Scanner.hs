{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Rosalia.Scanner where

import Control.Monad (void)
import Data.Char (ord)
import Data.String.Conversions
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

rword :: T.Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> space

rws :: [T.Text]
rws = ["λ", "let", "in", "where", "do"]

-- | lexeme p parses p, discarding trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | symbol s parses s
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

strlit :: Parser T.Text
strlit = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack (read ('"' : cs content ++ "\""))

charlit :: Parser Int
charlit =
  squotes $
    (ord <$> satisfy (`notElem` ['\\', '\'']))
      <|> (single '\\' >> int)

identifier :: Parser T.Text
identifier = (lexeme . try) (p >>= check)
  where
    p =
      fmap T.pack $
        (:)
          <$> letterChar
          <*> many (alphaNumChar <|> single '_')
    check x =
      if x `elem` rws
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else return x

-- | parses integer literals
int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

star :: Parser ()
star = void $ symbol "*"

-- | parses boolean literals
bool :: Parser Bool
bool = lexeme $ (True <$ rword "true") <|> (False <$ rword "false")

-- | space consumer
--
--   line-comment ::= --.*\n
--
--   block-comment ::= "{-" .* "-}"
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"
