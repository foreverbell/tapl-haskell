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
  '.'      { TokenDot }
  ','      { TokenComma }
  ':'      { TokenColon }
  '='      { TokenEq }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  '{'      { TokenLCurly }
  '}'      { TokenRCurly }
  '->'     { TokenArrow }
  var      { TokenVar $$ }
  'if'     { TokenIf }
  'then'   { TokenThen }
  'else'   { TokenElse }
  'true'   { TokenTrue }
  'false'  { TokenFalse }
  'lambda' { TokenLambda }
  'as'     { TokenAs }
  'Top'    { TokenTop }
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
  : PathTerm               { $1 }
  | AppTerm PathTerm       { TermApp $1 $2 }

PathTerm :: { Term }
  : PathTerm '.' var       { TermProj $1 $3 }
  | AscribeTerm            { $1 }

AscribeTerm :: { Term }
  : AtomicTerm             { $1 }
  | AtomicTerm 'as' Type   { TermAscribe $1 $3 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | 'true'                 { TermTrue }
  | 'false'                { TermFalse }
  | '{' Fields '}'         { TermRecord $2 }
  | var                    {% do { index <- nameToIndex $1; return (TermVar index); } }

Fields :: { [(String, Term)] }
  : Field                  { [$1] }
  | Field ',' Fields       { $1 : $3 }

Field :: { (String, Term) }
  : var '=' Term           { ($1, $3) }

Type :: { TermType }
  : ArrowType              { $1 }

ArrowType :: { TermType }
  : AtomicType '->' ArrowType
                           { TypeArrow $1 $3 }
  | AtomicType             { $1 }

AtomicType :: { TermType }
  : '(' Type ')'           { $2 }
  | 'Top'                  { TypeTop }
  | 'Bool'                 { TypeBool }
  | '{' FieldTypes '}'     { TypeRecord $2 }

FieldTypes :: { [(String, TermType)] }
  : FieldType              { [$1] }
  | FieldType ',' FieldTypes
                           { $1 : $3 }

FieldType :: { (String, TermType) }
  : var ':' Type           { ($1, $3) }

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
parseTree str = evalState (parse (scanTokens str)) C.makeEmptyContext

parseError :: [Token] -> a
parseError _ = error "parse error"

}
