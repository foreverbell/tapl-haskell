module Evaluator (
  evaluate
) where

import Data.List (find)

import Base

termMap :: (Int -> Int -> Term) -> Term -> Term
termMap onvar t = go 0 t
  where
    go index (TermVar var) = onvar index var
    go index (TermIfThenElse t1 t2 t3) = TermIfThenElse (go index t1) (go index t2) (go index t3)
    go index (TermAbs var ty t) = TermAbs var ty (go (index + 1) t)
    go index (TermApp t1 t2) = TermApp (go index t1) (go index t2)
    go index (TermRecord fields) = TermRecord (map (\(f, t) -> (f, go index t)) fields)
    go index (TermProj t field) = TermProj (go index t) field
    go index (TermAscribe t ty) = TermAscribe (go index t) ty
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse

-- | Shift up deBruijn indices of all free variables by delta.
termShift :: Term -> Int -> Term
termShift t delta = termMap (\index var -> if var >= index then TermVar (var + delta) else TermVar var) t

-- | Substitute the variable with 0 deBruijn index in term to subterm.
termSubstitute :: Term -> Term -> Term
termSubstitute t subt = termMap (\index var -> if var == index then termShift subt index else TermVar var) t

isValue :: Term -> Bool
isValue TermTrue = True
isValue TermFalse = True
isValue (TermRecord fields) = all (\(_, t) -> isValue t) fields
isValue (TermAbs _ _ _) = True
isValue _ = False

-- | One step evaluation, return Nothing if there is no rule applies.
evaluate1 :: Term -> Maybe Term

{- E-IfTrue -}
evaluate1 (TermIfThenElse TermTrue t1 _) = Just t1

{- E-IfFalse -}
evaluate1 (TermIfThenElse TermFalse _ t2) = Just t2

{- E-If -}
evaluate1 (TermIfThenElse t t1 t2) = TermIfThenElse <$> evaluate1 t <*> pure t1 <*> pure t2

{- E-Rcd -}
evaluate1 (TermRecord fields) = TermRecord <$> go fields
  where
    go [] = Nothing
    go ((f, t) : rest)
      | isValue t = do
          rest' <- go rest
          return ((f, t) : rest')
      | otherwise = do
          t' <- evaluate1 t
          return ((f, t') : rest)

{- E-ProjRcd -}
evaluate1 (TermProj v@(TermRecord fields) f)
  | isValue v = case find (\(f1, _) -> f1 == f) fields of
                  Just (_, t) -> Just t
                  Nothing -> undefined

{- E-Proj -}
evaluate1 (TermProj t f) = TermProj <$> evaluate1 t <*> pure f

{- E-AppAbs -}
evaluate1 (TermApp (TermAbs _ _ t) v)
  | isValue v = Just $ termShift (termSubstitute t (termShift v 1)) (-1)

{- E-App2 -}
evaluate1 (TermApp v t)
  | isValue v = TermApp v <$> evaluate1 t

{- E-App1 -}
evaluate1 (TermApp t1 t2) = TermApp <$> evaluate1 t1 <*> pure t2

{- E-NoRule -}
evaluate1 _ = Nothing

evaluate :: Term -> Term
evaluate t = case evaluate1 t of
  Just t' -> evaluate t'
  Nothing -> t
