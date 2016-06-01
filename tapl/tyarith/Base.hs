module Base (
  Token (..)
, Term (..)
, TermType (..)
) where

data Token
  = TokenInt Int
  | TokenIf | TokenThen | TokenElse
  | TokenSucc | TokenPred | TokenIsZero
  | TokenTrue | TokenFalse
  | TokenLBracket | TokenRBracket
  | TokenError
  deriving (Show)

data Term
  = TermZero | TermTrue | TermFalse
  | TermIsZero Term
  | TermIfThenElse Term Term Term
  | TermSucc Term | TermPred Term
  deriving (Show)

data TermType
  = TypeNat
  | TypeBool
  deriving (Eq, Show)
