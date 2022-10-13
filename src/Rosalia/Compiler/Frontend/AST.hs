module Rosalia.Compiler.Backend.AST where

import Data.Text (Text)

-- | bin-op ::= + | - | * | / | % | = | > | < | >= | <= | = | /= | & | |
data RosaBinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | Neq
  | And
  | Or
  | Greater
  | Less
  | Geq
  | Leq
  deriving (Show, Eq)

newtype RosaTypeSig
  = RTypeSig Text
  deriving (Show, Eq)

data RosaUnary
  = RNot
  | RNeg
  deriving (Show, Eq)

data RosaExpr
  = Literal Int
  | Fliteral Double
  | Boolit Bool
  | StrLit Text
  | CharLit Int
  | BinOp RosaBinOp RosaExpr RosaExpr
  | Id Text
  | Unary RosaUnary RosaExpr
  | Call Text [RosaExpr]
  | Sig RosaTypeSig
  | Parens RosaExpr
  | Decl RosaDecl
  | Assign RosaExpr RosaExpr
  deriving (Show, Eq)

data RosaType
  = RIntType
  | RBoolType
  | RCharType
  | RStringType
  | RUnitType
  deriving (Show, Eq)

data RosaDecl
  = Let Text RosaExpr
  | Type Text RosaType
  | Fun Text [Text] RosaExpr
  deriving (Show, Eq)

data Program = Program [RosaExpr] deriving (Eq, Show)