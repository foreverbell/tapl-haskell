module Types ( 
  Line, Column, LocationInfo
, Located (..)
, unLocate
, Token (..)
, Term (..)
) where

type Line = Int
type Column = Int
type LocationInfo = (Line, Column)

data Located a = Located LocationInfo a
  deriving (Show)

unLocate :: Located a -> a
unLocate (Located _ x) = x

data Token 
  = TokenInt Int
  | TokenIf | TokenThen | TokenElse
  | TokenSucc | TokenPred | TokenIsZero
  | TokenTrue | TokenFalse
  | TokenLBracket | TokenRBracket
  | TokenError
  deriving (Show)

data Term
  = TermZero | TermTrue | TermFalse
  | TermIsZero Term
  | TermIfThenElse Term Term Term
  | TermSucc Term | TermPred Term
  deriving (Show)
