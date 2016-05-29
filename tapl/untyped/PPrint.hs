module PPrint (
  pprint
) where

import Context
import Types

import Text.Printf (printf)

type Term = PolyTerm DeBruijn

pprint :: Term -> String
pprint = pprintTerm makeEmpty

pprintTerm :: Context -> Term -> String
pprintTerm ctx (TermAbs var term) = printf "lambda %s. %s" fresh (pprintTerm ctx' term)
  where
    (ctx', fresh) = pickFreshName ctx var
pprintTerm ctx term = pprintAppTerm ctx term

pprintAppTerm :: Context -> Term -> String
pprintAppTerm ctx (TermApp term1 term2) = printf "%s %s" (pprintAppTerm ctx term1) (pprintAtomicTerm ctx term2)
pprintAppTerm ctx term = pprintAtomicTerm ctx term

pprintAtomicTerm :: Context -> Term -> String
pprintAtomicTerm ctx (TermVar index) = indexToName ctx index
pprintAtomicTerm ctx term = printf "(%s)" (pprintTerm ctx term)
