module Evaluator (
  eval
) where

import DeBruijn (shift, substitue)
import Types

type Term = PolyTerm DeBruijn

-- | call-by-value evalutation strategy, treat abstraction term as value
-- | a redex is reducible only if its right-hand is evaluated to value
isValue :: Term -> Bool
isValue (TermAbs _ _) = True
isValue _ = False

eval1 :: Term -> Maybe Term

{- E-AppAbs -}
eval1 (TermApp (TermAbs _ t) v)
  | isValue v = Just $ shift (substitue t v) (-1)

{- E-App2 -}
eval1 (TermApp v t)
  | isValue v = do
      t' <- eval1 t
      return $ TermApp v t'

{- E-App1 -}
eval1 (TermApp t1 t2) = do
  t1' <- eval1 t1
  return $ TermApp t1' t2

{- E-NoRule -}
eval1 _ = Nothing

evalFull :: Term -> Either String Term
evalFull t = case eval1 t of
  Just t' -> if t' == t
               then Left "evaluation diverge"
               else evalFull t'
  Nothing -> Right t

eval :: Term -> Either String Term
eval term = do
  nf <- evalFull term
  if isValue nf
    then return nf
    else Left "evaluation error"
