{

module Parser (
  happyParseTerms
) where

import Type

}

%name happyParseTerms Term
%tokentype { Located Token }
%monad { Either String }
%error { parseError }

%token
  '('      { Located _ TokenLBracket }
  ')'      { Located _ TokenRBracket }
  int      { Located _ (TokenInt $$) }
  'if'     { Located _ TokenIf }
  'then'   { Located _ TokenThen }
  'else'   { Located _ TokenElse }
  'succ'   { Located _ TokenSucc }
  'pred'   { Located _ TokenPred }
  'iszero' { Located _ TokenIsZero }
  'true'   { Located _ TokenTrue }
  'false'  { Located _ TokenFalse }

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

parseError :: [Located Token] -> Either String a
parseError [] = Left $ "parse error at end of line"
parseError ((Located (_, column) _):_) = Left $ "parse error at column " ++ show column

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

}
