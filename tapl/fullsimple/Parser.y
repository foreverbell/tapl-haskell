{

module Parser (
  parseTree
) where

import           Control.Monad.State

import qualified Context as C
import           Lexer (scanTokens)
import           Base

}

%name parse Topmost
%tokentype { Token }
%monad { Parser }
%error { parseError }

%token
  '.'      { TokenDot }
  ','      { TokenComma }
  ':'      { TokenColon }
  ';'      { TokenSemi }
  '='      { TokenEq }
  '|'      { TokenVBar }
  '_'      { TokenUScore }
  '<'      { TokenLT }
  '>'      { TokenGT }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  '{'      { TokenLCurly }
  '}'      { TokenRCurly }
  '->'     { TokenArrow }
  '==>'    { TokenDDArrow }
  int      { TokenInt $$ }
  lcid     { TokenLCaseId $$ }
  ucid     { TokenUCaseId $$ }
  'if'     { TokenIf }
  'then'   { TokenThen }
  'else'   { TokenElse }
  'true'   { TokenTrue }
  'false'  { TokenFalse }
  'pred'   { TokenPred }
  'succ'   { TokenSucc }
  'iszero' { TokenIsZero }
  'unit'   { TokenUnit }
  'lambda' { TokenLambda }
  'let'    { TokenLet }
  'in'     { TokenIn }
  'letrec' { TokenLetrec }
  'type'   { TokenTypeAlias }
  'as'     { TokenAs }
  'case'   { TokenCase }
  'of'     { TokenOf }
  'Bool'   { TokenBool }
  'Nat'    { TokenNat }
  'Unit'   { TokenUUnit }

%%

Topmost :: { [Statement] }
  :                        { [] }
  | Statement ';' Topmost  { $1 : $3 }

Statement :: { Statement }
  : Term                   { Eval $1 }
  | 'type' ucid '=' Type   {% do { addName $2; return (BindType $2 $4); } }
  | 'let' LetBinder        {% do { let (pat, t, _) = $2 in return (BindLet pat t); } }
  | 'letrec' LetrecBinder  {% do { let (v, t, ty) = $2 in return (BindLet (PatternVar v) (TermFix (TermAbs v ty t))); } }

Term :: { Term }
  : AppTerm                { $1 }
  | 'lambda' TypedBinder '.' Term
                           {% do { dropOneName; let (v, ty) = $2 in return (TermAbs v ty $4); } }
  | 'if' Term 'then' Term 'else' Term
                           { TermIfThenElse $2 $4 $6 }
  | 'let' LetBinder 'in' Term
                           {% do { let (v, t, n) = $2 in (dropNames n >> return (TermLet v t $4)) } }
  | 'letrec' LetrecBinder 'in' Term
                           {% do { dropOneName; let (v, t, ty) = $2 in return (TermLet (PatternVar v) (TermFix (TermAbs v ty t)) $4); } }

TypedBinder :: { (String, TermType) }
  : lcid ':' Type          {% do { addName $1; return ($1, $3); } }
  | '_' ':' Type           {% do { addName "_"; return ("_", $3); } }

Pattern :: { Pattern }
  : lcid                   { PatternVar $1 }
  | '_'                    { PatternVar "_" }
  | '{' FieldPatterns '}'  { PatternRecord $2 }

FieldPatterns :: { [(String, Pattern)] }
  : FieldPattern           { [$1] }
  | FieldPattern ',' FieldPatterns
                           { ($1 : $3) }

FieldPattern :: { (String, Pattern) }
  : Pattern '=' lcid       { ($3, $1) }

LetBinder :: { (Pattern, Term, Int) }
  : Pattern '=' Term       {% do { count <- addPatternName $1; return ($1, $3, count); } }

LetrecBinder :: { (String, Term, TermType) }
  : TypedBinder '=' Term   { (fst $1, $3, snd $1) }

AppTerm :: { Term }
  : PathTerm               { $1 }
  | AppTerm PathTerm       { TermApp $1 $2 }
  | 'succ' PathTerm        { TermSucc $2 }
  | 'pred' PathTerm        { TermPred $2 }
  | 'iszero' PathTerm      { TermIsZero $2 }

PathTerm :: { Term }
  : PathTerm '.' lcid      { TermProj $1 $3 }
  | AscribeTerm            { $1 }

AscribeTerm :: { Term }
  : AtomicTerm             { $1 }
  | AtomicTerm 'as' Type   { TermAscribe $1 $3 }

AtomicTerm :: { Term }
  : '(' Term ')'           { $2 }
  | 'true'                 { TermTrue }
  | 'false'                { TermFalse }
  | int                    { intToTerm $1 }
  | 'unit'                 { TermUnit }
  | '{' Fields '}'         { TermRecord $2 }
  | lcid                   {% do { index <- nameToIndex $1; return (TermVar index); } }

Fields :: { [(String, Term)] }
  : Field                  { [$1] }
  | Field ',' Fields       { $1 : $3 }

Field :: { (String, Term) }
  : lcid '=' Term          { ($1, $3) }

Type :: { TermType }
  : ArrowType              { $1 }

ArrowType :: { TermType }
  : AtomicType '->' ArrowType
                           { TypeArrow $1 $3 }
  | AtomicType             { $1 }

AtomicType :: { TermType }
  : '(' Type ')'           { $2 }
  | 'Bool'                 { TypeBool }
  | 'Nat'                  { TypeNat }
  | 'Unit'                 { TypeUnit }
  | '{' FieldTypes '}'     { TypeRecord $2 }
  | ucid                   {% do { index <- nameToIndex $1; return (TypeVar index); } }

FieldTypes :: { [(String, TermType)] }
  : FieldType              { [$1] }
  | FieldType ',' FieldTypes
                           { $1 : $3 }

FieldType :: { (String, TermType) }
  : lcid ':' Type          { ($1, $3) }

{

intToTerm :: Int -> Term
intToTerm 0 = TermZero
intToTerm n = TermSucc $ intToTerm (n - 1)

addPatternName :: Pattern -> Parser Int
addPatternName (PatternVar var) = do { addName var; return 1; }
addPatternName (PatternRecord pats) = sum <$> forM pats (addPatternName . snd)

type Parser = State Context

nameToIndex :: String -> Parser Int
nameToIndex name = do
  ctx <- get
  return $ C.nameToIndex ctx name

addName :: String -> Parser ()
addName name = do
  ctx <- get
  put $ C.addName ctx name

dropNames :: Int -> Parser ()
dropNames n = modify (\ctx -> C.dropBindings ctx n)

dropOneName :: Parser ()
dropOneName = modify C.dropOneBinding

parseTree :: Context -> String -> [Statement]
parseTree ctx str = evalState (parse (scanTokens str)) ctx

parseError :: [Token] -> a
parseError _ = error "parse error"

}
