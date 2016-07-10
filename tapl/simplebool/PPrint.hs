module PPrint (
  pprint
, pprintType
) where

import Base
import Context

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
pprintAppTerm ctx (TermApp t1 t2) = printf "%s %s" (pprintAppTerm ctx t1) (pprintAtomicTerm ctx t2)
pprintAppTerm ctx t = pprintAtomicTerm ctx t

pprintAtomicTerm :: Context -> Term -> String
pprintAtomicTerm ctx (TermVar index) = fst $ indexToName ctx index
pprintAtomicTerm _ TermTrue = "true"
pprintAtomicTerm _ TermFalse = "false"
pprintAtomicTerm ctx t = printf "(%s)" (pprintTerm ctx t)

pprintType :: TermType -> String
pprintType = pprintArrowType

pprintArrowType :: TermType -> String
pprintArrowType (TypeArrow ty1 ty2) = printf "%s->%s" (pprintAtomicType ty1) (pprintArrowType ty2)
pprintArrowType ty = pprintAtomicType ty

pprintAtomicType :: TermType -> String
pprintAtomicType TypeBool = "Bool"
pprintAtomicType ty = printf "(%s)" (pprintType ty)
