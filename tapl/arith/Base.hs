{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Base (
  Token (..)
, Term (..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Token
  = TokenInt Int
  | TokenIf | TokenThen | TokenElse
  | TokenSucc | TokenPred | TokenIsZero
  | TokenTrue | TokenFalse
  | TokenLBracket | TokenRBracket
  deriving (Show)

data Term
  = TermZero | TermTrue | TermFalse
  | TermIsZero Term
  | TermIfThenElse Term Term Term
  | TermSucc Term | TermPred Term
  deriving (Show, Generic, NFData)
