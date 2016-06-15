module Base (
  Token (..)
, RE (..)
, SetItem (..)
, nub'
) where

import Data.List (group, sort)

data Token
  = TokenChar Char
  | TokenDot
  | TokenPlus | TokenHyphen | TokenStar
  | TokenCircumflex
  | TokenVBar
  | TokenOBrack | TokenCBrack
  | TokenOParen | TokenCParen
  deriving (Show)

data RE
  = REConcatenation RE RE
  | REUnion RE RE
  | REKleeneStar RE
  | REKleenePlus RE
  | REAnyChar
  | REChar Char
  | REPositiveSet [SetItem]
  | RENegativeSet [SetItem]
  deriving (Show)

data SetItem
  = ItemRange Char Char
  | ItemChar Char
  deriving (Show)

nub' :: (Ord a, Eq a) => [a] -> [a]
nub' = map head . group . sort
