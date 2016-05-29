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
pprintTerm ctx (TermAbs var t) = printf "lambda %s. %s" fresh (pprintTerm ctx' t)
  where
    (ctx', fresh) = pickFreshName ctx var
pprintTerm ctx t = pprintAppTerm ctx t

pprintAppTerm :: Context -> Term -> String
pprintAppTerm ctx (TermApp t1 t2) = printf "%s %s" (pprintAppTerm ctx t1) (pprintAtomicTerm ctx t2)
pprintAppTerm ctx t = pprintAtomicTerm ctx t

pprintAtomicTerm :: Context -> Term -> String
pprintAtomicTerm ctx (TermVar index) = indexToName ctx index
pprintAtomicTerm ctx t = printf "(%s)" (pprintTerm ctx t)
