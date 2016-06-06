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
  = BindVar TermType
  | BindTermAlias Term (Maybe TermType)
  | BindTypeAlias TermType
  | BindDeBruijn
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
  | TokenLambda | TokenLet | TokenIn | TokenTypeAlias | TokenAs | TokenCase | TokenOf
  | TokenArrow | TokenDDArrow
  | TokenDot | TokenComma | TokenColon | TokenSemi | TokenEq | TokenVBar | TokenUScore
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
  | TermRecord [(String, Term)]
  | TermProj Term String  -- TODO: Projection index sets to integer would be better.
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
  | TypeRecord [(String, TermType)]
  | TypeArrow TermType TermType
  | TypeVar Int -- ^ user-defined alias
  deriving (Show)
