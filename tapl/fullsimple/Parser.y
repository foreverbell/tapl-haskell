{

module Parser (
  parseTree
) where

import Lexer (scanTokens)
import Base

}

%name parse Term
%tokentype { Token }
%error { parseError }

%token
  '.'      { TokenDot }
  ','      { TokenComma }
  ':'      { TokenColon }
  ';'      { TokenSemi }
  '='      { TokenEq }
  '|'      { TokenVar }
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
  'alias'  { TokenAlias }
  'Bool'   { TokenBool }
  'Nat'    { TokenNat }
  'Unit'   { TokenUUnit }

%%

Term :: { Term }
  : AppTerm                           { $1 }
  | 'lambda' var ':' Type '.' Term    { TermAbs $2 $4 $6 }
  | 'if' Term 'then' Term 'else' Term { TermIfThenElse $2 $4 $6 }

AppTerm :: { Term }
  : AtomicTerm             { $1 }
  | AppTerm AtomicTerm     { TermApp $1 $2 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | var                    { TermVar $1 }
  | 'true'                 { TermTrue }
  | 'false'                { TermFalse }

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
  | ucid                   { undefined }
  | '{' FieldsType '}'     { undefined }
{

type Term = PolyTerm Parsed

parseTree :: String -> Term
parseTree = parse . scanTokens

parseError :: [Token] -> a
parseError _ = error "parse error"

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

}
