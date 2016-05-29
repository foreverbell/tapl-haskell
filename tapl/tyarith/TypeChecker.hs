module TypeChecker (
  typeCheck
) where

import Types

typeCheck :: Term -> TermType

{- T-True -}
typeCheck TermTrue = TypeBool

{- T-False -}
typeCheck TermFalse = TypeBool

{- T-If -}
typeCheck (TermIfThenElse t t1 t2) = case typeCheck t of
  TypeBool -> if ty1 == ty2 
                then ty1
                else error "type error: arms of conditional have different types"
  _ -> error "type error: guard of conditional not a boolean"
  where
    ty1 = typeCheck t1
    ty2 = typeCheck t2

{- T-Zero -}
typeCheck TermZero = TypeNat

{- T-Succ -}
typeCheck (TermSucc t) = case typeCheck t of
  TypeNat -> TypeNat
  _ -> error "type error: argument of succ is not a number"

{- T-Pred -}
typeCheck (TermPred t) = case typeCheck t of
  TypeNat -> TypeNat
  _ -> error "type error: argument of pred is not a number"

{- T-IsZero -}
typeCheck (TermIsZero t) = case typeCheck t of
  TypeNat -> TypeBool
  _ -> error "type error: argument of iszero is not a number"
