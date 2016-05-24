{

module Parser (
  parseTree
) where

import           Lexer (scanTokens)
import           Types

}

%name parse Term
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
  '('      { TokenLBracket }
  ')'      { TokenRBracket }
  var      { TokenVar $$ } 
  'lambda' { TokenLambda }
  '.'      { TokenDot }

%%

Term :: { Term Raw }
  : AppTerm                { $1 }
  | 'lambda' var '.' Term  { TermAbs $2 $4 }

AppTerm :: { Term Raw }
  : AtomTerm               { $1 }
  | AppTerm AtomTerm       { TermApp $1 $2 }

AtomTerm :: { Term Raw }
  : '(' Term ')'           { $2 }
  | var                    { TermVar $1 }

{

parseTree :: String -> Either String (Term Raw)
parseTree str = parse =<< scanTokens str

parseError :: [Token] -> Either String a
parseError _ = Left "parse error"

}
