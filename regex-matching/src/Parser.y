{

module Parser (
  happyParseRE
) where

import Types

}

%name happyParseRE RE
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
  '.'      { TokenDot }
  '+'      { TokenPlus }
  '-'      { TokenHyphen }
  '*'      { TokenStar }
  '^'      { TokenCircumflex }
  '|'      { TokenVBar }
  '('      { TokenOParen }
  ')'      { TokenCParen }
  '['      { TokenOBrack }
  ']'      { TokenCBrack }
  char     { TokenChar $$ }

%%

RE :: { RE }
  : RE '|' SimpleRE   { REUnion $1 $3 }
  | SimpleRE          { $1 }

SimpleRE :: { RE }
  : SimpleRE BasicRE  { REConcatenation $1 $2 }
  | BasicRE           { $1 }

BasicRE :: { RE }
  : Star              { $1 }
  | Plus              { $1 }
  | ElementaryRE      { $1 }

Star :: { RE }
  : ElementaryRE '*'  { REKleeneStar $1 }

Plus :: { RE }
  : ElementaryRE '+'  { REKleenePlus $1 }

ElementaryRE :: { RE }
  : Group             { $1 }
  | Any               { $1 }
  | Char              { $1 }
  | Set               { $1 }

Group :: { RE }
  : '(' RE ')'        { $2 }

Any :: { RE }
  : '.'               { REAnyChar }

Char :: { RE }
  : char              { REChar $1 }

Set :: { RE }
  : '[' SetItems ']'     { REPositiveSet $2 }
  | '[' '^' SetItems ']' { RENegativeSet $3 }

SetItems :: { [SetItem] }
  : SetItem           { [$1] }
  | SetItems SetItem  { $1 ++ [$2] }

SetItem :: { SetItem }
  : char              { ItemChar $1 }
  | char '-' char     { ItemRange $1 $3 }

{

parseError :: [Token] -> Either String a
parseError _ = Left "parse error"

}
