{

module Lexer (
  scanTokens
) where

import TAPL.Alex.Base
import Base

}

tokens :-
  $white+                ;
  \(                     { \_ -> TokenLParen }
  \)                     { \_ -> TokenRParen }
  \.                     { \_ -> TokenDot }
  \:                     { \_ -> TokenColon }
  \-\>                   { \_ -> TokenArrow }
  [a-zA-Z]+              { \s -> case lookupKeyword s of
                                   Just t -> t
                                   Nothing -> TokenVar s }
  [a-z][a-zA-Z0-9\_\']*  { \s -> TokenVar s }

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

lookupKeyword :: String -> Maybe Token
lookupKeyword kw = lookup kw keywords
  where
    keywords = [ ("if", TokenIf)
               , ("then", TokenThen)
               , ("else", TokenElse)
               , ("true", TokenTrue)
               , ("false", TokenFalse)
               , ("Bool", TokenBool)
               , ("lambda", TokenLambda)
               ]

}
