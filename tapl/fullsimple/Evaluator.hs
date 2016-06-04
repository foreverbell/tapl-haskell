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
isValue TermUnit = True
isValue (TermAbs _ _ _) = True
isValue t = isNumericValue t

evaluate :: Context -> Term -> Term
evaluate = undefined

evaluateBinding :: Context -> Binding -> Binding
evaluateBinding ctx (BindTermAlias t) = BindTermAlias (evaluate ctx t)
evaluateBinding _ binding = binding
