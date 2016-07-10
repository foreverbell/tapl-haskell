{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Base (
  Context (..)
, Token (..)
, Term (..)
, TermType (..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

newtype Context = Context [(String, TermType)]
  deriving (Show)

data Token
  = TokenIf | TokenThen | TokenElse
  | TokenTrue | TokenFalse
  | TokenTop | TokenArrow | TokenBool
  | TokenLambda | TokenVar String | TokenDot
  | TokenAs
  | TokenComma | TokenColon | TokenEq
  | TokenLParen | TokenRParen
  | TokenLCurly | TokenRCurly
  deriving (Show, Generic, NFData)

data Term
  = TermIfThenElse Term Term Term
  | TermTrue | TermFalse
  | TermRecord [(String, Term)]
  | TermProj Term String
  | TermVar Int
  | TermAbs String TermType Term
  | TermApp Term Term
  | TermAscribe Term TermType
  deriving (Show, Generic, NFData)

data TermType
  = TypeTop
  | TypeBool
  | TypeRecord [(String, TermType)]
  | TypeArrow TermType TermType
  deriving (Show, Generic, NFData)
