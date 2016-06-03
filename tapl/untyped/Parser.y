{

module Parser (
  parseTree
) where

import           Control.Monad.State

import qualified Context as C
import           Lexer (scanTokens)
import           Base

}

%name parse Term
%tokentype { Token }
%monad { Parser }
%error { parseError }

%token
  '('      { TokenLBracket }
  ')'      { TokenRBracket }
  var      { TokenVar $$ } 
  'lambda' { TokenLambda }
  '.'      { TokenDot }

%%

Term :: { Term }
  : AppTerm                      { $1 }
  | 'lambda' BinderVar '.' Term  {% do { dropHeadName; return (TermAbs $2 $4); } }

BinderVar :: { String }
  : var                    {% do { addName $1; return $1; } }

AppTerm :: { Term }
  : AtomicTerm             { $1 }
  | AppTerm AtomicTerm     { TermApp $1 $2 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | var                    {% do { index <- nameToIndex $1; return (TermVar index); } }

{

type Parser = State Context

nameToIndex :: String -> Parser Int
nameToIndex name = do
  ctx <- get
  return $ C.nameToIndex ctx name

addName :: String -> Parser ()
addName name = do
  ctx <- get
  put $ C.addName ctx name

dropHeadName :: Parser ()
dropHeadName = modify C.dropHeadName

parseTree :: String -> Term
parseTree str = evalState (parse tokens) C.makeEmpty 
  where tokens = scanTokens str

parseError :: [Token] -> a
parseError _ = error "parse error"

}
