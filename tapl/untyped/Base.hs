{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Base (
  Context (..)
, Token (..)
, Term (..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

newtype Context = Context [String]
  deriving (Show)

data Token
  = TokenLambda
  | TokenVar String
  | TokenDot
  | TokenLBracket | TokenRBracket
  deriving (Show, Generic, NFData)

data Term
  = TermVar Int
  | TermAbs String Term
  | TermApp Term Term
  deriving (Show, Eq, Generic, NFData)
