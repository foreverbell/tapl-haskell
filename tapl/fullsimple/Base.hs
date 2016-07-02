{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Base (
  Statement (..)
, Binding (..)
, Context (..)
, Token (..)
, Pattern (..)
, Term (..)
, TermType (..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Statement
  = Eval Term
  | BindType String TermType
  | BindLet Pattern Term
  deriving (Show, Generic, NFData)

data Binding
  = BindVar TermType
  | BindTermAlias Term TermType
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
  | TokenNil | TokenCons
  | TokenIsNil | TokenHead | TokenTail
  | TokenUnit
  | TokenBool | TokenNat | TokenList | TokenUUnit
  | TokenLambda | TokenLet | TokenIn | TokenLetrec | TokenTypeAlias | TokenAs
  | TokenArrow
  | TokenDot | TokenComma | TokenColon | TokenSemi | TokenEq | TokenUScore
  | TokenLParen | TokenRParen
  | TokenLCurly | TokenRCurly
  | TokenLBracket | TokenRBracket
  deriving (Show, Generic, NFData)

data Pattern
  = PatternVar String
  | PatternRecord [(String, Pattern)]
  deriving (Show, Generic, NFData)

data Term
  = TermIfThenElse Term Term Term
  | TermTrue | TermFalse
  | TermSucc Term | TermPred Term | TermIsZero Term
  | TermZero
  | TermNil TermType | TermCons Term Term
  | TermIsNil Term | TermHead Term | TermTail Term
  | TermUnit
  | TermRecord [(String, Term)]
  | TermProj Term String
  | TermLet Pattern Term Term
  | TermFix Term
  | TermVar Int
  | TermAbs String TermType Term
  | TermApp Term Term
  | TermAscribe Term TermType
  deriving (Show, Generic, NFData)

data TermType
  = TypeBool
  | TypeNat
  | TypeList TermType
  | TypeUnit
  | TypeRecord [(String, TermType)]
  | TypeArrow TermType TermType
  | TypeVar Int -- user-defined alias
  deriving (Show, Generic, NFData)
