module TypeChecker (
  typeCheck
) where

import Types

typeCheck :: Term -> Either String Type

{- T-True -}
typeCheck TermTrue = Right TypeBool

{- T-False -}
typeCheck TermFalse = Right TypeBool

{- T-If -}
typeCheck (TermIfThenElse t t1 t2) = do
  ty <- typeCheck t
  ty1 <- typeCheck t1
  ty2 <- typeCheck t2
  case ty of
    TypeBool -> if ty1 == ty2
                   then Right ty1
                   else Left "type error: arms of conditional have different types"
    _ -> Left "type error: guard of conditional not a boolean"

{- T-Zero -}
typeCheck TermZero = Right TypeNat

{- T-Succ -}
typeCheck (TermSucc t) = do
  ty <- typeCheck t
  case ty of
    TypeNat -> Right TypeNat
    _ -> Left "type error: argument of succ is not a number"

{- T-Pred -}
typeCheck (TermPred t) = do
  ty <- typeCheck t
  case ty of
    TypeNat -> Right TypeNat
    _ -> Left "type error: argument of pred is not a number"

{- T-IsZero -}
typeCheck (TermIsZero t) = do
  ty <- typeCheck t
  case ty of
    TypeNat -> Right TypeBool
    _ -> Left "type error: argument of iszero is not a number"
