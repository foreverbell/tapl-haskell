{

module Parser (
  happyParseExp
, Exp (..)
) where

import Lexer (Token (..), Located (..))

}

%name happyParseExp Exp
%tokentype { Located Token }
%monad { Either String }
%error { parseError }

%token
  '(' { Located _ TokenLB }
  ')' { Located _ TokenRB }
  '+' { Located _ (TokenBinOp '+') }
  '-' { Located _ (TokenBinOp '-') }
  '*' { Located _ (TokenBinOp '*') }
  int { Located _ (TokenInt $$) }

%%

Exp :: { Exp }
  : Exp '+' Term    { Plus $1 $3 }
  | Exp '-' Term    { Minus $1 $3 }
  | Term            { $1 }

Term :: { Exp }
  : Term '*' Factor { Mult $1 $3}
  | Factor          { $1 }

Factor :: { Exp }
  : int             { Int $1 }
  | '(' Exp ')'     { $2 }

{

parseError :: [Located Token] -> Either String a
parseError [] = Left $ "parse error at end of line" -- TODO: what is the line?
parseError ((Located (line, column) _):_) = Left $ "parse error at line " ++ show line ++ ", column " ++ show column

data Exp = Plus Exp Exp
         | Minus Exp Exp
         | Mult Exp Exp
         | Int Integer
  deriving (Show)

}
