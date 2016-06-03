{

module Parser (
  parseTree
) where

import           Control.Monad.State

import qualified Context as C
import           Lexer (scanTokens)
import           Base

}

%name parse Topmost
%tokentype { Token }
%monad { Parser }
%error { parseError }

%token
  '.'      { TokenDot }
  ','      { TokenComma }
  ':'      { TokenColon }
  ';'      { TokenSemi }
  '='      { TokenEq }
  '|'      { TokenVBar }
  '<'      { TokenLT }
  '>'      { TokenGT }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  '{'      { TokenLCurly }
  '}'      { TokenRCurly }
  '->'     { TokenArrow }
  '==>'    { TokenDDArrow }
  int      { TokenInt $$ }
  lcid     { TokenLCaseId $$ }
  ucid     { TokenUCaseId $$ }
  'if'     { TokenIf }
  'then'   { TokenThen }
  'else'   { TokenElse }
  'true'   { TokenTrue }
  'false'  { TokenFalse }
  'pred'   { TokenPred }
  'succ'   { TokenSucc }
  'iszero' { TokenIsZero }
  'unit'   { TokenUnit }
  'lambda' { TokenLambda }
  'let'    { TokenLet }
  'in'     { TokenIn }
  'as'     { TokenAs }
  'case'   { TokenCase }
  'of'     { TokenOf }
  'Bool'   { TokenBool }
  'Nat'    { TokenNat }
  'Unit'   { TokenUUnit }

%%

Topmost :: { [Command] }
  :                        { [] }
  | Command ';' Topmost    { $1 : $3 }

Command :: { Command }
  : Term                   { Eval $1 }
  | lcid '=' Term          {% do { addName $1; return (Bind $1 (TermAliasBind $3)); } }
  | ucid '=' Type          {% do { addName $1; return (Bind $1 (TypeAliasBind $3)); } }

Term :: { Term }
  : AppTerm                                  { $1 }
  | 'lambda' LambdaBinder ':' Type '.' Term  {% do { dropHeadName; return (TermAbs $2 $4 $6); } }
  | 'if' Term 'then' Term 'else' Term        { TermIfThenElse $2 $4 $6 }
  | 'let' LetBinder 'in' Term                {% do { dropHeadName; let (v, t) = $2 in return (TermLet v t $4); } }

LambdaBinder :: { String }
  : lcid                   {% do { addName $1; return $1; } }

LetBinder :: { (String, Term) }
  : lcid '=' Term          {% do { addName $1; return ($1, $3); } }

AppTerm :: { Term }
  : AscribeTerm            { $1 }
  | AppTerm AscribeTerm    { TermApp $1 $2 }
  | 'succ' AscribeTerm     { TermSucc $2 }
  | 'pred' AscribeTerm     { TermPred $2 }
  | 'iszero' AscribeTerm   { TermIsZero $2 }

AscribeTerm :: { Term }
  : AtomicTerm             { $1 }
  | AtomicTerm 'as' Type   { TermAscribe $1 $3 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | 'true'                 { TermTrue }
  | 'false'                { TermFalse }
  | int                    { intToTerm $1 }
  | 'unit'                 { TermUnit }
  | lcid                   {% do { index <- nameToIndex $1; return (TermVar index); } }

Type :: { TermType }
  : ArrowType              { $1 }

ArrowType :: { TermType }
  : AtomicType '->' ArrowType { TypeArrow $1 $3 }
  | AtomicType                { $1 }

AtomicType :: { TermType }
  : '(' Type ')'           { $2 }
  | 'Bool'                 { TypeBool }
  | 'Nat'                  { TypeNat }
  | 'Unit'                 { TypeUnit }
  | ucid                   {% do { index <- nameToIndex $1; return (TypeId index); } }
--  | '{' FieldsType '}'     { undefined }

{

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

type Parser = State Context

nameToIndex :: String -> Parser Int
nameToIndex name = do
  ctx <- get
  return $ C.nameToIndex ctx name

addName :: String -> Parser ()
addName name = do
  ctx <- get
  put $ C.addBinding ctx name DeBruijnBind

dropHeadName :: Parser ()
dropHeadName = modify C.dropHeadBinding

parseTree :: String -> [Command]
parseTree str = evalState (parse tokens) C.makeEmpty
  where tokens = scanTokens str

parseError :: [Token] -> a
parseError _ = error "parse error"

}
