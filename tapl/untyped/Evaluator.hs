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
    go cutoff (TermAbs var t) = TermAbs var (go (cutoff + 1) t)
    go cutoff (TermApp t1 t2) = TermApp (go cutoff t1) (go cutoff t2)

-- | Substitute the variable with 0 deBruijn index in term to subterm.
termSubstitute :: Term -> Term -> Term
termSubstitute t subt = go 0 subt t
  where
    go :: Int -> Term -> Term -> Term
    go index subt (TermVar var)
      | var == index = subt
      | otherwise = TermVar var
    go index subt (TermAbs var t) = TermAbs var (go (index + 1) (termShift subt 1) t)
    go index subt (TermApp t1 t2) = TermApp (go index subt t1) (go index subt t2)

-- | Call-by-value evalutation strategy, treat abstraction term as value.
-- | A redex is reducible only if its right-hand is evaluated to value.
isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _ = False

evaluate1 :: Term -> Maybe Term

{- E-AppAbs -}
evaluate1 (TermApp (TermAbs _ t) v)
  | isValue v = Just $ termShift (termSubstitute t (termShift v 1)) (-1)

{- E-App2 -}
evaluate1 (TermApp v t)
  | isValue v = TermApp v <$> evaluate1 t

{- E-App1 -}
evaluate1 (TermApp t1 t2) = TermApp <$> evaluate1 t1 <*> pure t2

{- E-NoRule -}
evaluate1 _ = Nothing

evaluateToNF :: Term -> Term
evaluateToNF t = case evaluate1 t of
  Just t' -> if t' == t then error "evaluation error: diverges" else evaluateToNF t'
  Nothing -> t

evaluate :: Term -> Term
evaluate term = if isValue nf then nf else error "evaluation error"
  where nf = evaluateToNF term
