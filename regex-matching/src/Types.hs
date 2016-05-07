module Types ( 
  Token (..)
, RE (..)
, SetItem (..)
, TransitionRule (..)
, Edge, NFA (..), unNFA
) where

import qualified Data.Sequence as S

data Token 
  = TokenChar Char
  | TokenDot                            -- .
  | TokenPlus | TokenHyphen | TokenStar -- +-*
  | TokenCircumflex                     -- ^
  | TokenVBar                           -- |
  | TokenOBrack | TokenCBrack           -- []
  | TokenOParen | TokenCParen           -- ()
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

data TransitionRule
  = TransEpsilon
  | TransAny
  | TransChar Char
  | TransPositive [TransitionRule]
  | TransNegative [TransitionRule]
  | TransRange Char Char
  deriving (Show)

{- Nondeterministic finite automaton, node is indexed from 0,
 - for concatenating NFA conveniently, starting node is set to 0,
 - and termination node is set to (n-1).
-}

type Edge a = (Int, a)

newtype NFA = NFA (S.Seq [Edge TransitionRule])
  deriving (Show)

unNFA :: NFA -> (S.Seq [Edge TransitionRule], Int)
unNFA (NFA nfa) = (nfa, S.length nfa)
