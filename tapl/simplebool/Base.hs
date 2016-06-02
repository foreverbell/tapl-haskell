module Base (
  Context (..)
, Token (..)
, Term (..)
, TermType (..)
) where

newtype Context = Context [(String, TermType)]
  deriving (Show)

data Token
  = TokenIf | TokenThen | TokenElse
  | TokenTrue | TokenFalse
  | TokenArrow | TokenBool
  | TokenColon
  | TokenLambda | TokenVar String | TokenDot
  | TokenLBracket | TokenRBracket
  deriving (Show)

data Term
  = TermIfThenElse Term Term Term
  | TermTrue | TermFalse
  | TermVar Int
  | TermAbs String TermType Term
  | TermApp Term Term
  deriving (Eq, Show)

data TermType
  = TypeBool
  | TypeArrow TermType TermType
  deriving (Eq, Show)
