module DeBruijn (
  deBruijn
, shift
, substitute
) where

import qualified Context as C
import           Types

type Term = PolyTerm DeBruijn

deBruijn :: PolyTerm Parsed -> Either String Term
deBruijn term = go C.empty term
  where
    go :: Context -> PolyTerm Parsed -> Either String Term
    go ctx (TermVar var) = TermVar <$> C.nameToIndex ctx var
    go ctx (TermAbs var term) = TermAbs var <$> go (C.addName ctx var) term
    go ctx (TermApp term1 term2) = TermApp <$> go ctx term1 <*> go ctx term2

shift :: Term -> Int -> Term
shift term delta = go 0 term
  where
    go :: Int -> Term -> Term
    go cutoff (TermVar var)
      | var >= cutoff = TermVar (var + delta)
      | otherwise = TermVar var
    go cutoff (TermAbs var term) = TermAbs var (go (cutoff + 1) term)
    go cutoff (TermApp term1 term2) = TermApp (go cutoff term1) (go cutoff term2)

-- | substitute variable with deBruijn index 0 in term to subterm.
substitute :: Term -> Term -> Term
substitute term subterm = go 0 subterm term
  where
    go :: Int -> Term -> Term -> Term
    go index subterm (TermVar var)
      | var == index = subterm
      | otherwise = TermVar var
    go index subterm (TermAbs var term) = TermAbs var (go (index + 1) (shift subterm 1) term)
    go index subterm (TermApp term1 term2) = TermApp (go index subterm term1) (go index subterm term2)
