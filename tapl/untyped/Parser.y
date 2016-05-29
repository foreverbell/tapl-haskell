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
  var      { TokenVar $$ } 
  'lambda' { TokenLambda }
  '.'      { TokenDot }

%%

Term :: { Term }
  : AppTerm                { $1 }
  | 'lambda' var '.' Term  { TermAbs $2 $4 }

AppTerm :: { Term }
  : AtomicTerm             { $1 }
  | AppTerm AtomicTerm     { TermApp $1 $2 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | var                    { TermVar $1 }

{

type Term = PolyTerm Parsed

parseTree :: String -> Term
parseTree = parse . scanTokens

parseError :: [Token] -> a
parseError _ = error "parse error"

}
