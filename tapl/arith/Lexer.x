{

module Lexer (
  scanTokens
) where

import TAPL.Alex
import Base

}

tokens :-
  $white+    ;
  [0-9]+     { \s -> TokenInt (read s) }
  [a-zA-Z]+  { \s -> case lookupKeyword s of
                       Just token -> token
                       Nothing -> alexFail }
  \(         { \_ -> TokenLBracket }
  \)         { \_ -> TokenRBracket }

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
      AlexToken inp' length action -> action (take length input) :  go inp'

lookupKeyword :: String -> Maybe Token
lookupKeyword kw = lookup kw keywords
  where
    keywords = [ ("if", TokenIf)
               , ("then", TokenThen)
               , ("else", TokenElse)
               , ("succ", TokenSucc)
               , ("pred", TokenPred)
               , ("iszero", TokenIsZero)
               , ("true", TokenTrue)
               , ("false", TokenFalse)
               ]

}
