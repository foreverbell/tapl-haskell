{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
  Context (..)
, Token (..)
, Term (..)
, Raw, DeBruijn
) where

newtype Context = Context [String]

data Token
  = TokenLambda
  | TokenVar String
  | TokenDot
  | TokenLBracket | TokenRBracket
  deriving (Show)

data Raw
data DeBruijn

type family TermVarType t where
  TermVarType Raw = String
  TermVarType DeBruijn = Int

data Term a
  = TermVar (TermVarType a)
  | TermAbs String (Term a)
  | TermApp (Term a) (Term a)

deriving instance (Show (TermVarType a)) => Show (Term a) 
