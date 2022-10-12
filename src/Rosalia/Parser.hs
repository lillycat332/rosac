{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Rosalia.Parser where

import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

data CompBinOp
  = Greater
  | Less
  deriving (Show)

data ArithExpr
  = Var T.Text
  | IntLit Integer
  | Neg ArithExpr
  | ArithBinary ArithBinaryOp ArithExpr ArithExpr
  deriving (Show)

-- arith-op ::= + | - | * | /
data ArithBinaryOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"