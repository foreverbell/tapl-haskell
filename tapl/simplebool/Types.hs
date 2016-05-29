{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types (
  Context (..)
, Token (..)
, PolyTerm (..)
, Parsed, DeBruijn
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

data Parsed
data DeBruijn

type family TermVarType t where
  TermVarType Parsed = String
  TermVarType DeBruijn = Int

data PolyTerm a
  = TermIfThenElse (PolyTerm a) (PolyTerm a) (PolyTerm a)
  | TermTrue | TermFalse
  | TermVar (TermVarType a)
  | TermAbs String TermType (PolyTerm a)
  | TermApp (PolyTerm a) (PolyTerm a)

deriving instance Show (TermVarType a) => Show (PolyTerm a) 
deriving instance Eq (TermVarType a) => Eq (PolyTerm a)

data TermType
  = TypeBool
  | TypeArrow TermType TermType
  deriving (Eq, Show)
