module Evaluator (
  evaluate
) where

import DeBruijn (shift, substitute)
import Types

type Term = PolyTerm DeBruijn

-- | Call-by-value evalutation strategy, treat abstraction term as value.
-- | A redex is reducible only if its right-hand is evaluated to value.
isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _ = False

evaluate1 :: Term -> Maybe Term

{- E-AppAbs -}
evaluate1 (TermApp (TermAbs _ t) v)
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

evaluateToNF :: Term -> Term
evaluateToNF t = case evaluate1 t of
  Just t' -> if t' == t then error "evaluation error: diverges" else evaluateToNF t'
  Nothing -> t

evaluate :: Term -> Term
evaluate term = if isValue nf then nf else error "evaluation error"
  where nf = evaluateToNF term
