{

module Parser (
  parseTree
) where

import Lexer (scanTokens)
import Types

}

%name parse Term
%tokentype { Token }
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

{

type Term = PolyTerm Parsed

parseTree :: String -> Term
parseTree = parse . scanTokens

parseError :: [Token] -> a
parseError _ = error "parse error"

}
