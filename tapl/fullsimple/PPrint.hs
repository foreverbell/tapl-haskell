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
pprintTerm ctx (TermLet pat t1 t2) = printf "let %s=%s in %s" pp (pprintTerm ctx t1) (pprintTerm ctx' t2)
  where
    (ctx', pp) = pprintPattern ctx pat
    pprintPattern :: Context -> Pattern -> (Context, String)
    pprintPattern ctx (PatternVar var) = pickFreshName ctx var
    pprintPattern ctx (PatternRecord pats) = let (ctx', pp) = go ctx pats in (ctx', printf "{%s}" (tail pp))
      where
        go :: Context -> [(String, Pattern)] -> (Context, String)
        go ctx [] = (ctx, [])
        go ctx ((index, pat) : pats) = (ctx'', printf ",%s=%s%s" pp index pp')
          where
            (ctx', pp) = pprintPattern ctx pat
            (ctx'', pp') = go ctx' pats
pprintTerm ctx t = pprintAppTerm ctx t

pprintAppTerm :: Context -> Term -> String
pprintAppTerm ctx (TermApp t1 t2) = printf "%s %s" (pprintAppTerm ctx t1) (pprintPathTerm ctx t2)
pprintAppTerm ctx (TermFix t) = printf "fix %s" (pprintAppTerm ctx t)
pprintAppTerm ctx t0@(TermSucc t) = case pprintNat t0 of
  Just p -> p
  Nothing -> printf "succ %s" (pprintPathTerm ctx t)
pprintAppTerm ctx t0@(TermPred t) = case pprintNat t0 of
  Just p -> p
  Nothing -> printf "pred %s" (pprintPathTerm ctx t)
pprintAppTerm ctx (TermIsZero t) = printf "iszero %s" (pprintPathTerm ctx t)
pprintAppTerm ctx (TermCons t1 t2) = printf "cons %s %s" (pprintPathTerm ctx t1) (pprintPathTerm ctx t2)
pprintAppTerm ctx (TermIsNil t) = printf "isnil %s" (pprintPathTerm ctx t)
pprintAppTerm ctx (TermHead t) = printf "head %s" (pprintPathTerm ctx t)
pprintAppTerm ctx (TermTail t) = printf "tail %s" (pprintPathTerm ctx t)
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
pprintAtomicTerm ctx (TermNil ty) = printf "nil[%s]" (pprintType ctx ty)
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
pprintAtomicType ctx (TypeList ty) = printf "List[%s]" (pprintType ctx ty)
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
