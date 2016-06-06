module PPrint (
  pprint
, pprintType
) where

import Base
import Context (indexToBinding, pickFreshName)

import Data.List (intercalate)
import Text.Printf (printf)

pprint :: Context -> Term -> String
pprint ctx t = pprintTerm ctx t

pprintTerm :: Context -> Term -> String
pprintTerm ctx (TermAbs var ty t) = printf "lambda %s:%s. %s" fresh (pprintType ctx ty) (pprintTerm ctx' t)
  where
    (ctx', fresh) = pickFreshName ctx var
pprintTerm ctx (TermIfThenElse t1 t2 t3) = printf "if %s then %s else %s" (pprintTerm ctx t1) (pprintTerm ctx t2) (pprintTerm ctx t3)
pprintTerm ctx (TermLet var t1 t2) = printf "let %s=%s in %s" fresh (pprintTerm ctx t1) (pprintTerm ctx' t2)
  where
    (ctx', fresh) = pickFreshName ctx var
pprintTerm ctx t = pprintAppTerm ctx t

pprintAppTerm :: Context -> Term -> String
pprintAppTerm ctx (TermApp t1 t2) = printf "%s %s" (pprintAppTerm ctx t1) (pprintPathTerm ctx t2)
pprintAppTerm ctx t0@(TermSucc t) = case pprintNat t0 of
  Just p -> p
  Nothing -> printf "succ %s" (pprintPathTerm ctx t)
pprintAppTerm ctx t0@(TermPred t) = case pprintNat t0 of
  Just p -> p
  Nothing -> printf "pred %s" (pprintPathTerm ctx t)
pprintAppTerm ctx (TermIsZero t) = printf "iszero %s" (pprintPathTerm ctx t)
pprintAppTerm ctx t = pprintPathTerm ctx t

pprintPathTerm :: Context -> Term -> String
pprintPathTerm ctx (TermProj t field) = printf "%s.%s" (pprintPathTerm ctx t) field
pprintPathTerm ctx t = pprintAscribeTerm ctx t

pprintAscribeTerm :: Context -> Term -> String
pprintAscribeTerm ctx (TermAscribe t ty) = printf "%s as %s" (pprintAtomicTerm ctx t) (pprintType ctx ty)
pprintAscribeTerm ctx t = pprintAtomicTerm ctx t

pprintAtomicTerm :: Context -> Term -> String
pprintAtomicTerm _ TermTrue = "true"
pprintAtomicTerm _ TermFalse = "false"
pprintAtomicTerm _ TermZero = "0"
pprintAtomicTerm _ TermUnit = "unit"
pprintAtomicTerm ctx (TermRecord fields) = printf "{%s}" (pprintFields ctx fields)
pprintAtomicTerm ctx (TermVar var) = fst $ indexToBinding ctx var
pprintAtomicTerm ctx t = printf "(%s)" (pprintTerm ctx t)

pprintFields :: Context -> [(String, Term)] -> String
pprintFields ctx fields = intercalate "," (map (\(f, t) -> f ++ "=" ++ pprintTerm ctx t) fields)

pprintType :: Context -> TermType -> String
pprintType ctx ty = pprintArrowType ctx ty

pprintArrowType :: Context -> TermType -> String
pprintArrowType ctx (TypeArrow ty1 ty2) = printf "%s->%s" (pprintAtomicType ctx ty1) (pprintArrowType ctx ty2)
pprintArrowType ctx ty = pprintAtomicType ctx ty

pprintAtomicType :: Context -> TermType -> String
pprintAtomicType _ TypeBool = "Bool"
pprintAtomicType _ TypeNat = "Nat"
pprintAtomicType _ TypeUnit = "Unit"
pprintAtomicType ctx (TypeRecord fields) = printf "{%s}" (pprintFieldTypes ctx fields)
pprintAtomicType ctx (TypeVar var) = fst $ indexToBinding ctx var
pprintAtomicType ctx t = printf "(%s)" (pprintType ctx t)

pprintFieldTypes :: Context -> [(String, TermType)] -> String
pprintFieldTypes ctx fields = intercalate "," (map (\(f, t) -> f ++ ":" ++ pprintType ctx t) fields)

pprintNat :: Term -> Maybe String
pprintNat t = show <$> go t
  where
    go :: Term -> Maybe Int
    go TermZero = Just 0
    go (TermSucc nv) = succ <$> go nv
    go _ = Nothing
