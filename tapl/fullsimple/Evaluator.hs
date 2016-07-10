module Evaluator (
  evaluate
, unpackPattern
) where

import Base
import Context

import Data.List (find)

termMap :: (Int -> Int -> Term) -> Term -> Term
termMap onvar t = go 0 t
  where
    go index (TermIfThenElse t t1 t2) = TermIfThenElse (go index t) (go index t1) (go index t2)
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse
    go index (TermSucc t) = TermSucc (go index t)
    go index (TermPred t) = TermPred (go index t)
    go index (TermIsZero t) = TermIsZero (go index t)
    go _ TermZero = TermZero
    go _ (TermNil ty) = TermNil ty
    go index (TermCons t1 t2) = TermCons (go index t1) (go index t2)
    go index (TermIsNil t) = TermIsNil (go index t)
    go index (TermHead t) = TermHead (go index t)
    go index (TermTail t) = TermTail (go index t)
    go _ TermUnit = TermUnit
    go index (TermRecord fields) = TermRecord (map (\(f, t) -> (f, go index t)) fields)
    go index (TermProj t field) = TermProj (go index t) field
    go index (TermLet var t1 t2) = TermLet var (go index t1) (go (index + 1) t2)
    go index (TermFix t) = TermFix (go index t)
    go index (TermVar var) = onvar index var
    go index (TermAbs var ty t) = TermAbs var ty (go (index + 1) t)
    go index (TermApp t1 t2) = TermApp (go index t1) (go index t2)
    go index (TermAscribe t ty) = TermAscribe (go index t) ty

-- | Shift up deBruijn indices of all free variables by delta.
termShift :: Term -> Int -> Term
termShift t delta = termMap (\index var -> if var >= index then TermVar (var + delta) else TermVar var) t

-- | Substitute the variable with 0 deBruijn index in term to subterm.
termSubstitute :: Term -> Term -> Term
termSubstitute t subt = termMap (\index var -> if var == index then termShift subt index else TermVar var) t

-- | The real substitution, i.e. [0 -> subt] t.
termSubstituteTop :: Term -> Term -> Term
termSubstituteTop t subt = termShift (termSubstitute t (termShift subt 1)) (-1)

getBindingTerm :: Context -> Int -> Term
-- One shall notice that deBruijn index is **relative** to current context,
-- so when moving one binding to another context, don't forget to relocate (shift by index+1) all term variable indices.
getBindingTerm ctx index = termShift t (index + 1)
  where
    t = case snd (indexToBinding ctx index) of
          BindTermAlias t _ -> t
          _ -> undefined

isNumericValue :: Term -> Bool
isNumericValue TermZero = True
isNumericValue (TermSucc t) = isNumericValue t
isNumericValue _ = False

isValue :: Term -> Bool
isValue TermTrue = True
isValue TermFalse = True
isValue (TermNil _) = True
isValue (TermCons t1 t2) = isValue t1 && isValue t2
isValue TermUnit = True
isValue (TermRecord fields) = all (\(_, t) -> isValue t) fields
isValue (TermAbs _ _ _) = True
isValue t = isNumericValue t

-- | Extract all variables and their binding terms.
-- The result list element order shall be consistent with `addPatternName` in Parser.y,
-- so the following variable substitutions are executed in the correct order.
unpackPattern :: Term -> Pattern -> [(String, Term)]
unpackPattern t (PatternVar var) = [(var, t)]
unpackPattern (TermRecord fields) (PatternRecord pats) = go pats
  where
    go :: [(String, Pattern)] -> [(String, Term)]
    go [] = []
    go ((index, pat) : pats) = case find (\(index2, _) -> index == index2) fields of
                                 Just (_, t) -> go pats ++ unpackPattern t pat
                                 _ -> undefined
unpackPattern _ _ = undefined

evaluate1 :: Context -> Term -> Maybe Term

evaluate1 _ (TermIfThenElse TermTrue t1 _) = Just t1

evaluate1 _ (TermIfThenElse TermFalse _ t2) = Just t2

evaluate1 ctx (TermIfThenElse t t1 t2) = TermIfThenElse <$> evaluate1 ctx t <*> pure t1 <*> pure t2

evaluate1 ctx (TermSucc t) = TermSucc <$> evaluate1 ctx t

evaluate1 _ (TermPred TermZero) = Just TermZero

evaluate1 _ (TermPred (TermSucc nv))
  | isNumericValue nv = Just nv

evaluate1 ctx (TermPred t) = TermPred <$> evaluate1 ctx t

evaluate1 _ (TermIsZero TermZero) = Just TermTrue

evaluate1 _ (TermIsZero (TermSucc nv))
  | isNumericValue nv = Just TermFalse

evaluate1 ctx (TermIsZero t) = TermIsZero <$> evaluate1 ctx t

evaluate1 ctx (TermCons v1 t2)
  | isValue v1 = TermCons v1 <$> evaluate1 ctx t2

evaluate1 ctx (TermCons t1 t2) = TermCons <$> evaluate1 ctx t1 <*> pure t2

evaluate1 _ (TermIsNil (TermNil _)) = Just TermTrue

evaluate1 _ (TermIsNil (TermCons v1 v2))
  | isValue v1 && isValue v2 = Just TermFalse

evaluate1 ctx (TermIsNil t) = TermIsNil <$> evaluate1 ctx t

evaluate1 _ (TermHead (TermNil _)) = error "evaluation error: head of empty list"

evaluate1 _ (TermHead (TermCons v1 v2))
  | isValue v1 && isValue v2 = Just v1

evaluate1 ctx (TermHead t) = TermHead <$> evaluate1 ctx t

evaluate1 _ (TermTail (TermNil _)) = error "evaluation error: tail of empty list"

evaluate1 _ (TermTail (TermCons v1 v2))
  | isValue v1 && isValue v2 = Just v2

evaluate1 ctx (TermTail t) = TermTail <$> evaluate1 ctx t

evaluate1 ctx (TermRecord fields) = TermRecord <$> go ctx fields
  where
    go _ [] = Nothing
    go ctx ((f, t) : rest)
      | isValue t = do
          rest' <- go ctx rest
          return ((f, t) : rest')
      | otherwise = do
          t' <- evaluate1 ctx t
          return ((f, t') : rest)

evaluate1 _ (TermProj v@(TermRecord fields) f)
  | isValue v = case find (\(f1, _) -> f1 == f) fields of
                  Just (_, t) -> Just t
                  Nothing -> undefined

evaluate1 ctx (TermProj t f) = TermProj <$> evaluate1 ctx t <*> pure f

evaluate1 _ (TermLet pat v t)
  | isValue v = Just $ foldl termSubstituteTop t (snd <$> unpackPattern v pat)

evaluate1 ctx (TermLet pat t1 t2) = TermLet pat <$> evaluate1 ctx t1 <*> pure t2

evaluate1 _ t1@(TermFix (TermAbs _ _ t2)) = Just $ termSubstituteTop t2 t1

evaluate1 ctx (TermFix t) = TermFix <$> evaluate1 ctx t

evaluate1 ctx (TermVar var) = Just $ getBindingTerm ctx var

evaluate1 _ (TermApp (TermAbs _ _ t) v)
  | isValue v = Just $ termSubstituteTop t v

evaluate1 ctx (TermApp v t)
  | isValue v = TermApp v <$> evaluate1 ctx t

evaluate1 ctx (TermApp t1 t2) = TermApp <$> evaluate1 ctx t1 <*> pure t2

evaluate1 _ (TermAscribe v _)
  | isValue v = Just v

evaluate1 ctx (TermAscribe t ty) = TermAscribe <$> evaluate1 ctx t <*> pure ty

evaluate1 _ _ = Nothing

evaluate :: Context -> Term -> Term
evaluate ctx t = case evaluate1 ctx t of
  Just t' -> evaluate ctx t'
  Nothing -> t
