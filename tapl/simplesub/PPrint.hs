module PPrint (
  pprint
, pprintType
) where

import Base
import Context

import Data.List (intercalate)
import Text.Printf (printf)

pprint :: Term -> String
pprint = pprintTerm makeEmptyContext

pprintTerm :: Context -> Term -> String
pprintTerm ctx (TermAbs var ty t) = printf "lambda %s:%s. %s" fresh (pprintType ty) (pprintTerm ctx' t)
  where
    (ctx', fresh) = pickFreshName ctx var ty
pprintTerm ctx (TermIfThenElse t1 t2 t3) = printf "if %s then %s else %s" (pprintTerm ctx t1) (pprintTerm ctx t2) (pprintTerm ctx t3)
pprintTerm ctx t = pprintAppTerm ctx t

pprintAppTerm :: Context -> Term -> String
pprintAppTerm ctx (TermApp t1 t2) = printf "%s %s" (pprintAppTerm ctx t1) (pprintPathTerm ctx t2)
pprintAppTerm ctx t = pprintPathTerm ctx t

pprintPathTerm :: Context -> Term -> String
pprintPathTerm ctx (TermProj t field) = printf "%s.%s" (pprintPathTerm ctx t) field
pprintPathTerm ctx t = pprintAscribeTerm ctx t

pprintAscribeTerm :: Context -> Term -> String
pprintAscribeTerm ctx (TermAscribe t ty) = printf "%s as %s" (pprintAtomicTerm ctx t) (pprintType ty)
pprintAscribeTerm ctx t = pprintAtomicTerm ctx t

pprintAtomicTerm :: Context -> Term -> String
pprintAtomicTerm ctx (TermVar index) = fst $ indexToName ctx index
pprintAtomicTerm _ TermTrue = "true"
pprintAtomicTerm _ TermFalse = "false"
pprintAtomicTerm ctx (TermRecord fields) = printf "{%s}" (pprintFields ctx fields)
pprintAtomicTerm ctx t = printf "(%s)" (pprintTerm ctx t)

pprintFields :: Context -> [(String, Term)] -> String
pprintFields ctx fields = intercalate "," (map (\(f, t) -> f ++ "=" ++ pprintTerm ctx t) fields)

pprintType :: TermType -> String
pprintType = pprintArrowType

pprintArrowType :: TermType -> String
pprintArrowType (TypeArrow ty1 ty2) = printf "%s->%s" (pprintAtomicType ty1) (pprintArrowType ty2)
pprintArrowType ty = pprintAtomicType ty

pprintAtomicType :: TermType -> String
pprintAtomicType TypeTop = "Top"
pprintAtomicType TypeBool = "Bool"
pprintAtomicType (TypeRecord fields) = printf "{%s}" (pprintFieldTypes fields)
pprintAtomicType ty = printf "(%s)" (pprintType ty)

pprintFieldTypes :: [(String, TermType)] -> String
pprintFieldTypes fields = intercalate "," (map (\(f, t) -> f ++ ":" ++ pprintType t) fields)
