{

module Parser (
  parseTree
) where

import Lexer (scanTokens)
import Types

}

%name parse Term
%tokentype { Token }
%monad { Either String }
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
  : AtomTerm          { $1 }
  | 'succ' AtomTerm   { TermSucc $2 }
  | 'pred' AtomTerm   { TermPred $2 }
  | 'iszero' AtomTerm { TermIsZero $2 }

AtomTerm :: { Term }
  : '(' Term ')'      { $2 }
  | 'true'            { TermTrue }
  | 'false'           { TermFalse }
  | int               { intToTerm $1 }

{

parseTree :: String -> Either String Term
parseTree str = parse =<< scanTokens str

parseError :: [Token] -> Either String a
parseError _ = Left "parse error"

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

}
