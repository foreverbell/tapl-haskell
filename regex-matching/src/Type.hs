module Type ( 
  Token (..)
, RE (..)
, SetItem (..)
) where

data Token 
  = TokenChar Char
  | TokenDot                  -- .
  | TokenPlus | TokenHyphen | TokenStar -- +-*
  | TokenCircumflex           -- ^
  | TokenVBar                 -- |
  | TokenOBrack | TokenCBrack -- []
  | TokenOParen | TokenCParen -- ()
  deriving (Show)

data RE 
  = REConcatenation RE RE
  | REAlternative RE RE
  | REStar RE
  | REPlus RE
  | REAnyChar
  | REChar Char
  | REPositiveSet [SetItem]
  | RENegativeSet [SetItem]
  deriving (Show)

data SetItem
  = ItemRange Char Char
  | ItemChar Char
  deriving (Show)
