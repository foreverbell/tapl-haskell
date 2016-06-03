module Base (
  Command (..)
, Binding (..)
, Context (..)
, Token (..)
, Term (..)
, TermType (..)
) where

data Command
  = Eval Term
  | Bind String Binding
  deriving (Show)

data Binding
  = VariableBind TermType
  | TermAliasBind Term
  | TypeAliasBind TermType
  | DeBruijnBind
  deriving (Show)

newtype Context = Context [(String, Binding)]
  deriving (Show)

data Token
  = TokenInt Int
  | TokenLCaseId String | TokenUCaseId String
  | TokenIf | TokenThen | TokenElse
  | TokenSucc | TokenPred | TokenIsZero
  | TokenTrue | TokenFalse | TokenZero
  | TokenUnit
  | TokenBool | TokenNat | TokenUUnit
  | TokenLambda | TokenLet | TokenIn | TokenAs | TokenCase | TokenOf
  | TokenArrow | TokenDDArrow
  | TokenDot | TokenComma | TokenColon | TokenSemi | TokenEq | TokenVBar
  | TokenLT | TokenGT
  | TokenLParen | TokenRParen
  | TokenLCurly | TokenRCurly
  deriving (Show)

data Term
  = TermIfThenElse Term Term Term
  | TermTrue | TermFalse
  | TermSucc Term | TermPred Term | TermIsZero Term
  | TermZero
  | TermUnit
  | TermLet String Term Term
  | TermVar Int
  | TermAbs String TermType Term
  | TermApp Term Term
  | TermAscribe Term TermType
  deriving (Show)

data TermType
  = TypeBool
  | TypeNat
  | TypeUnit
  | TypeArrow TermType TermType
  | TypeId Int  -- ^ user-defined alias
  deriving (Show)
