{

module Lexer (
  scanTokens
) where

import TAPL.Alex.Helper
import Base

}

tokens :-
  $white+                ;
  lambda                 { \_ -> TokenLambda }
  \.                     { \_ -> TokenDot }
  \(                     { \_ -> TokenLBracket }
  \)                     { \_ -> TokenRBracket }
  [a-z][a-zA-z0-9\_\']*  { \s -> TokenVar s }

{

type AlexAction = String -> Token

alexFail :: a
alexFail = error "lexical error"

scanTokens :: String -> [Token]
scanTokens str = go ('\n', [], str)
  where
    go inp@(_, _, input) = case alexScan inp 0 of
      AlexEOF -> []
      AlexError _ -> alexFail
      AlexSkip inp' _ -> go inp'
      AlexToken inp' length action -> action (take length input) : go inp'

}
