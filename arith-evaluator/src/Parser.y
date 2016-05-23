{

module Parser (
  parseTree
, Expr (..)
) where

import Lexer (scanTokens, Token (..), Located (..))

}

%name parse Expr
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

Expr :: { Expr }
  : Expr '+' Term    { Plus $1 $3 }
  | Expr '-' Term    { Minus $1 $3 }
  | Term            { $1 }

Term :: { Expr }
  : Term '*' Factor { Mult $1 $3}
  | Factor          { $1 }

Factor :: { Expr }
  : int             { Int $1 }
  | '(' Expr ')'     { $2 }

{

parseTree :: String -> Either String Expr
parseTree str = parse =<< scanTokens str

parseError :: [Located Token] -> Either String a
parseError [] = Left $ "parse error at end of line" -- TODO: what is the line?
parseError ((Located (line, column) _):_) = Left $ "parse error at line " ++ show line ++ ", column " ++ show column

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Int Integer
  deriving (Show)

}
