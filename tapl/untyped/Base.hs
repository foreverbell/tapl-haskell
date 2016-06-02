module Base (
  Context (..)
, Token (..)
, Term (..)
) where

newtype Context = Context [String]
  deriving (Show)

data Token
  = TokenLambda
  | TokenVar String
  | TokenDot
  | TokenLBracket | TokenRBracket
  deriving (Show)

data Term
  = TermVar Int
  | TermAbs String Term
  | TermApp Term Term
  deriving (Show, Eq)
