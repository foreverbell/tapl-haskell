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
  '.'      { TokenDot }
  ':'      { TokenColon }
  '->'     { TokenArrow }
  var      { TokenVar $$ }
  'lambda' { TokenLambda }
  'if'     { TokenIf }
  'then'   { TokenThen }
  'else'   { TokenElse }
  'true'   { TokenTrue }
  'false'  { TokenFalse }
  'Bool'   { TokenBool }

%%

Term :: { Term }
  : AppTerm                { $1 }
  | 'lambda' BinderVar ':' Type '.' Term
                           {% do { dropOneName; return (TermAbs $2 $4 $6); } }
  | 'if' Term 'then' Term 'else' Term
                           { TermIfThenElse $2 $4 $6 }

BinderVar :: { String }
  : var                    {% do { addName $1; return $1; } }

AppTerm :: { Term }
  : AtomicTerm             { $1 }
  | AppTerm AtomicTerm     { TermApp $1 $2 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | var                    {% do { index <- nameToIndex $1; return (TermVar index); } }
  | 'true'                 { TermTrue }
  | 'false'                { TermFalse }

Type :: { TermType }
  : ArrowType              { $1 }

ArrowType :: { TermType }
  : AtomicType '->' ArrowType
                           { TypeArrow $1 $3 }
  | AtomicType             { $1 }

AtomicType :: { TermType }
  : '(' Type ')'           { $2 }
  | 'Bool'                 { TypeBool }

{

type Parser = State Context

nameToIndex :: String -> Parser Int
nameToIndex name = do
  ctx <- get
  return $ C.nameToIndex ctx name

addName :: String -> Parser ()
addName name = do
  ctx <- get
  put $ C.addName ctx name undefined

dropOneName :: Parser ()
dropOneName = modify C.dropOneName

parseTree :: String -> Term
parseTree str = evalState (parse tokens) C.makeEmptyContext
  where tokens = scanTokens str

parseError :: [Token] -> a
parseError _ = error "parse error"

}
