{

module Parser (
  parseTree
, Expr (..)
) where

import Lexer (scanTokens, Token (..))

}

%name parse Expr
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
  '(' { TokenLB }
  ')' { TokenRB }
  '+' { TokenBinOp '+' }
  '-' { TokenBinOp '-' }
  '*' { TokenBinOp '*' }
  int { TokenInt $$ }

%%

Expr :: { Expr }
  : Expr '+' Term    { Plus $1 $3 }
  | Expr '-' Term    { Minus $1 $3 }
  | Term             { $1 }

Term :: { Expr }
  : Term '*' Factor  { Mult $1 $3 }
  | Factor           { $1 }

Factor :: { Expr }
  : int              { Int $1 }
  | '(' Expr ')'     { $2 }

{

parseTree :: String -> Either String Expr
parseTree str = parse =<< scanTokens str

parseError :: [Token] -> Either String a
parseError _ = Left "parse error"

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Int Integer
  deriving (Show)

}
