module DeBruijn (
  deBruijn
, shift
, substitute
) where

import Context
import Base

type Term = PolyTerm DeBruijn

deBruijn :: PolyTerm Parsed -> Term
deBruijn t = go makeEmpty t
  where
    go :: Context -> PolyTerm Parsed -> Term
    go ctx (TermVar var) = TermVar (nameToIndex ctx var)
    go ctx (TermAbs var t) = TermAbs var (go (addName ctx var) t)
    go ctx (TermApp t1 t2) = TermApp (go ctx t1) (go ctx t2)

shift :: Term -> Int -> Term
shift t delta = go 0 t
  where
    go :: Int -> Term -> Term
    go cutoff (TermVar var)
      | var >= cutoff = TermVar (var + delta)
      | otherwise = TermVar var
    go cutoff (TermAbs var t) = TermAbs var (go (cutoff + 1) t)
    go cutoff (TermApp t1 t2) = TermApp (go cutoff t1) (go cutoff t2)

-- | substitute variable with deBruijn index 0 in term to subterm.
substitute :: Term -> Term -> Term
substitute t subt = go 0 subt t
  where
    go :: Int -> Term -> Term -> Term
    go index subt (TermVar var)
      | var == index = subt
      | otherwise = TermVar var
    go index subt (TermAbs var t) = TermAbs var (go (index + 1) (shift subt 1) t)
    go index subt (TermApp t1 t2) = TermApp (go index subt t1) (go index subt t2)
