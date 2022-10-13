{-# LANGUAGE OverloadedStrings #-}

module Rosalia.Parser where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Combinators.Expr
import Data.Either
import Rosalia.AST
import Rosalia.Scanner
import Text.Megaparsec
import Text.Megaparsec.Char

opTable :: [[Operator Parser RosaExpr]]
opTable =
  [ [ unary (Unary RNeg) "-"
    ],
    [infixR Pow "^"],
    [infixL Mul "*", infixL Div "/"],
    [infixL Add "+", infixL Sub "-"],
    [infixL Leq "<=", infixL Geq ">=", infixL Less "<", infixL Greater ">"],
    [infixL' Eq "=", infixL Neq "/="],
    [infixL' And "&"],
    [infixL' Or "|"],
    [InfixR $ Assign <$ symbol ":="]
  ]
  where
    -- Megaparsec doesn't support multiple prefix operators by default,
    -- but we need this in order to parse things like double negatives,
    -- nots, and dereferences
    unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
    infixL op sym = InfixL $ BinOp op <$ symbol sym
    -- Primed infixL' is useful for operators which are prefixes of other operators
    infixL' op sym = InfixL $ BinOp op <$ operator sym
    infixR op sym = InfixR $ BinOp op <$ symbol sym
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("^+-*&|/<>:=" :: [Char])

termParser :: Parser RosaExpr
termParser =
  try (parens exprParser)
    <|> Literal <$> int
    <|> Fliteral <$> float
    <|> Boolit <$> bool
    <|> Id <$> identifier
    <|> StrLit <$> strlit
    <|> CharLit <$> charlit
    <|> Decl <$> declParser

-- <|> Decl <$> declParser

declParser :: Parser RosaDecl
declParser = do
  rword "let"
  name <- identifier
  _ <- symbol "="
  Let name <$> exprParser

exprParser :: Parser RosaExpr
exprParser = makeExprParser termParser opTable

programParser :: Parser Program
programParser = between sc eof $ do
  decls <- many declParser
  return $ Program decls