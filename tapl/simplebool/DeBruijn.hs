module DeBruijn (
  deBruijn
, shift
, substitute
) where

import Context
import Types

type Term = PolyTerm DeBruijn

deBruijn :: PolyTerm Parsed -> Term
deBruijn t = go makeEmpty t
  where
    go :: Context -> PolyTerm Parsed -> Term
    go ctx (TermVar var) = TermVar (nameToIndex ctx var)
    go ctx (TermAbs var ty t) = TermAbs var ty (go (addName ctx var ty) t)
    go ctx (TermApp t1 t2) = TermApp (go ctx t1) (go ctx t2)
    go ctx (TermIfThenElse t1 t2 t3) = TermIfThenElse (go ctx t1) (go ctx t2) (go ctx t3)
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse

shift :: Term -> Int -> Term
shift t delta = go 0 t
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

-- | substitute variable with deBruijn index 0 in term to subterm.
substitute :: Term -> Term -> Term
substitute t subt = go 0 subt t
  where
    go :: Int -> Term -> Term -> Term
    go index sub (TermVar var)
      | var == index = sub
      | otherwise = TermVar var
    go index subt (TermAbs var ty t) = TermAbs var ty (go (index + 1) (shift subt 1) t)
    go index subt (TermApp t1 t2) = TermApp (go index subt t1) (go index subt t2)
    go index subt (TermIfThenElse t1 t2 t3) = TermIfThenElse (go index subt t1) (go index subt t2) (go index subt t3)
    go _ _ TermTrue = TermTrue
    go _ _ TermFalse = TermFalse