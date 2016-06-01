module Evaluator (
  evaluate
) where

import Base

isNumericValue :: Term -> Bool
isNumericValue TermZero = True
isNumericValue (TermPred t) = isNumericValue t
isNumericValue (TermSucc t) = isNumericValue t
isNumericValue _ = False

isValue :: Term -> Bool
isValue TermTrue = True
isValue TermFalse = True
isValue t = isNumericValue t

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

{- E-Succ -}
evaluate1 (TermSucc t) = do
  t' <- evaluate1 t
  return $ TermSucc t'

{- E-PredZero -}
evaluate1 (TermPred TermZero) = Just TermZero

{- E-PredSucc -}
evaluate1 (TermPred (TermSucc nv))
  | isNumericValue nv = Just nv

{- E-Pred -}
evaluate1 (TermPred t) = do
  t' <- evaluate1 t
  return $ TermPred t'

{- E-IsZeroZero -}
evaluate1 (TermIsZero TermZero) = Just TermTrue

{- E-IsZeroSucc -}
evaluate1 (TermIsZero (TermSucc nv))
  | isNumericValue nv = Just TermFalse

{- E-IsZero -}
evaluate1 (TermIsZero t) = do
  t' <- evaluate1 t
  return $ TermIsZero t'

{- E-NoRule -}
evaluate1 _ = Nothing

evaluateToNF :: Term -> Term
evaluateToNF t = case evaluate1 t of
  Just t' -> evaluateToNF t'
  Nothing -> t

evaluate :: Term -> Term
evaluate t = if isValue nf then nf else error "evaluation error"
  where nf = evaluateToNF t
