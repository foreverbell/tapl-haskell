module Evaluator (
  evaluate
) where

import DeBruijn (shift, substitute)
import Types

type Term = PolyTerm DeBruijn

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
  | isValue v = Just $ shift (substitute t (shift v 1)) (-1)

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
