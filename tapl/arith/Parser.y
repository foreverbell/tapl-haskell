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
  '('      { TokenLBracket }
  ')'      { TokenRBracket }
  int      { TokenInt $$ }
  'if'     { TokenIf }
  'then'   { TokenThen }
  'else'   { TokenElse }
  'succ'   { TokenSucc }
  'pred'   { TokenPred }
  'iszero' { TokenIsZero }
  'true'   { TokenTrue }
  'false'  { TokenFalse }

%%

Term :: { Term }
  : AppTerm                           { $1 }
  | 'if' Term 'then' Term 'else' Term { TermIfThenElse $2 $4 $6 }

AppTerm :: { Term }
  : AtomicTerm                        { $1 }
  | 'succ' AtomicTerm                 { TermSucc $2 }
  | 'pred' AtomicTerm                 { TermPred $2 }
  | 'iszero' AtomicTerm               { TermIsZero $2 }

AtomicTerm :: { Term }
  : '(' Term ')'                      { $2 }
  | 'true'                            { TermTrue }
  | 'false'                           { TermFalse }
  | int                               { intToTerm $1 }

{

parseTree :: String -> Term
parseTree = parse . scanTokens

parseError :: [Token] -> a
parseError _ = error "parse error"

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

}
