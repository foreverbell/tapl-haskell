module DeBruijn (
  deBruijn
, shift
, substitute
) where

import Context
import Types

type Term = PolyTerm DeBruijn

deBruijn :: PolyTerm Parsed -> Term
deBruijn term = go makeEmpty term
  where
    go :: Context -> PolyTerm Parsed -> Term
    go ctx (TermVar var) = TermVar (nameToIndex ctx var)
    go ctx (TermAbs var ttype term) = TermAbs var ttype (go (addName ctx var ttype) term)
    go ctx (TermApp term1 term2) = TermApp (go ctx term1) (go ctx term2)
    go ctx (TermIfThenElse term1 term2 term3) = TermIfThenElse (go ctx term1) (go ctx term2) (go ctx term3)
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse

shift :: Term -> Int -> Term
shift term delta = go 0 term
  where
    go :: Int -> Term -> Term
    go cutoff (TermVar var)
      | var >= cutoff = TermVar (var + delta)
      | otherwise = TermVar var
    go cutoff (TermAbs var ttype term) = TermAbs var ttype (go (cutoff + 1) term)
    go cutoff (TermApp term1 term2) = TermApp (go cutoff term1) (go cutoff term2)
    go cutoff (TermIfThenElse term1 term2 term3) = TermIfThenElse (go cutoff term1) (go cutoff term2) (go cutoff term3)
    go _ TermTrue = TermTrue
    go _ TermFalse = TermFalse

-- | substitute variable with deBruijn index 0 in term to subterm.
substitute :: Term -> Term -> Term
substitute term subterm = go 0 subterm term
  where
    go :: Int -> Term -> Term -> Term
    go index sub (TermVar var)
      | var == index = sub
      | otherwise = TermVar var
    go index sub (TermAbs var ttype term) = TermAbs var ttype (go (index + 1) (shift sub 1) term)
    go index sub (TermApp term1 term2) = TermApp (go index sub term1) (go index sub term2)
    go index sub (TermIfThenElse term1 term2 term3) = TermIfThenElse (go index sub term1) (go index sub term2) (go index sub term3)
    go _ _ TermTrue = TermTrue
    go _ _ TermFalse = TermFalse
