{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Base (
  Context (..)
, Token (..)
, PolyTerm (..)
, Parsed, DeBruijn
) where

newtype Context = Context [String]
  deriving (Show)

data Token
  = TokenLambda
  | TokenVar String
  | TokenDot
  | TokenLBracket | TokenRBracket
  deriving (Show)

data Parsed
data DeBruijn

type family TermVarType t where
  TermVarType Parsed = String
  TermVarType DeBruijn = Int

data PolyTerm a
  = TermVar (TermVarType a)
  | TermAbs String (PolyTerm a)
  | TermApp (PolyTerm a) (PolyTerm a)

deriving instance Show (TermVarType a) => Show (PolyTerm a) 
deriving instance Eq (TermVarType a) => Eq (PolyTerm a) 
