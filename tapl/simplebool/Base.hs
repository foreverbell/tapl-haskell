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
  | TokenArrow | TokenBool
  | TokenColon
  | TokenLambda | TokenVar String | TokenDot
  | TokenLParen | TokenRParen
  deriving (Show, Generic, NFData)

data Term
  = TermIfThenElse Term Term Term
  | TermTrue | TermFalse
  | TermVar Int
  | TermAbs String TermType Term
  | TermApp Term Term
  deriving (Eq, Show, Generic, NFData)

data TermType
  = TypeBool
  | TypeArrow TermType TermType
  deriving (Eq, Show, Generic, NFData)
