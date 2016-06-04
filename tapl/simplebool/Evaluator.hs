module Evaluator (
  evaluate
) where

import Base

-- | Shift up deBruijn indices of all free variables by delta.
termShift :: Term -> Int -> Term
termShift t delta = go 0 t
  where
    go :: Int -> Term -> Term
    go cutoff (TermVar var)
      | var >= cutoff = TermVar (var + delta)
      | otherwise = TermVar var
    go cutoff (TermAbs var ty t) = TermAbs var ty (go (cutoff + 1) t)
    go cutoff (TermApp t1 t2) = TermApp (go cutoff t1) (go cutoff t2)
    go cutoff (TermIfThenElse t1 t2 t3) = TermIfThenElse (go cutoff t1) (go cutoff t2) (go cutoff t3)
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse

-- | Substitute the variable with 0 deBruijn index in term to subterm.
termSubstitute :: Term -> Term -> Term
termSubstitute t subt = go 0 subt t
  where
    go :: Int -> Term -> Term -> Term
    go index sub (TermVar var)
      | var == index = sub
      | otherwise = TermVar var
    go index subt (TermAbs var ty t) = TermAbs var ty (go (index + 1) (termShift subt 1) t)
    go index subt (TermApp t1 t2) = TermApp (go index subt t1) (go index subt t2)
    go index subt (TermIfThenElse t1 t2 t3) = TermIfThenElse (go index subt t1) (go index subt t2) (go index subt t3)
    go _ _ TermTrue = TermTrue
    go _ _ TermFalse = TermFalse

isValue :: Term -> Bool
isValue TermTrue = True
isValue TermFalse = True
isValue (TermAbs _ _ _) = True
isValue _ = False

-- | One step evaluation, return Nothing if there is no rule applies.
evaluate1 :: Term -> Maybe Term

{- E-IfTrue -}
evaluate1 (TermIfThenElse TermTrue t1 _) = Just t1

{- E-IfFalse -}
evaluate1 (TermIfThenElse TermFalse _ t2) = Just t2

{- E-If -}
evaluate1 (TermIfThenElse t t1 t2) = do
  t' <- evaluate1 t
  return $ TermIfThenElse t' t1 t2

{- E-AppAbs -}
evaluate1 (TermApp (TermAbs _ _ t) v)
  | isValue v = Just $ termShift (termSubstitute t (termShift v 1)) (-1)

{- E-App2 -}
evaluate1 (TermApp v t)
  | isValue v = do
      t' <- evaluate1 t
      return $ TermApp v t'

{- E-App1 -}
evaluate1 (TermApp t1 t2) = do
  t1' <- evaluate1 t1
  return $ TermApp t1' t2

{- E-NoRule -}
evaluate1 _ = Nothing

evaluate :: Term -> Term
evaluate t = case evaluate1 t of
  Just t' -> evaluate t'
  Nothing -> t
