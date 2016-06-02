{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Base (
  Context (..)
, Token (..)
, PolyTerm (..)
, Parsed, DeBruijn
, TermType (..)
) where

newtype Context = Context [(String, TermType)]
  deriving (Show)

data Token
  = TokenInt Int
  | TokenLCaseId String | TokenUCaseId String
  | TokenIf | TokenThen | TokenElse
  | TokenSucc | TokenPred | TokenIsZero
  | TokenTrue | TokenFalse | TokenZero
  | TokenUnit
  | TokenBool | TokenNat | TokenUUnit
  | TokenLambda | TokenLet | TokenIn | TokenAs | TokenCase | TokenOf | TokenAlias
  | TokenArrow | TokenDDArrow
  | TokenDot | TokenComma | TokenColon | TokenSemi | TokenEq | TokenVBar
  | TokenLT | TokenGT
  | TokenLParen | TokenRParen
  | TokenLCurly | TokenRCurly
  deriving (Show)

data Parsed
data DeBruijn

type family TermVarType t where
  TermVarType Parsed = String
  TermVarType DeBruijn = Int

data PolyTerm a
  = TermIfThenElse (PolyTerm a) (PolyTerm a) (PolyTerm a)
  | TermTrue | TermFalse
  | TermSucc (PolyTerm a) | TermPred (PolyTerm a) | TermIsZero (PolyTerm a)
  | TermZero
  | TermVar (TermVarType a)
  | TermAbs String TermType (PolyTerm a)
  | TermApp (PolyTerm a) (PolyTerm a)

deriving instance Show (TermVarType a) => Show (PolyTerm a) 
deriving instance Eq (TermVarType a) => Eq (PolyTerm a)

data TermType
  = TypeBool
  | TypeNat
  | TypeUnit
  | TypeArrow TermType TermType
  deriving (Eq, Show)
