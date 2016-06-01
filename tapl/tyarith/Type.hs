module Type (
  typeOf
) where

import Base

typeOf :: Term -> TermType

{- T-True -}
typeOf TermTrue = TypeBool

{- T-False -}
typeOf TermFalse = TypeBool

{- T-If -}
typeOf (TermIfThenElse t t1 t2) = case typeOf t of
  TypeBool -> if ty1 == ty2 
                then ty1
                else error "type error: arms of conditional have different types"
  _ -> error "type error: guard of conditional not a boolean"
  where
    ty1 = typeOf t1
    ty2 = typeOf t2

{- T-Zero -}
typeOf TermZero = TypeNat

{- T-Succ -}
typeOf (TermSucc t) = case typeOf t of
  TypeNat -> TypeNat
  _ -> error "type error: argument of succ is not a number"

{- T-Pred -}
typeOf (TermPred t) = case typeOf t of
  TypeNat -> TypeNat
  _ -> error "type error: argument of pred is not a number"

{- T-IsZero -}
typeOf (TermIsZero t) = case typeOf t of
  TypeNat -> TypeBool
  _ -> error "type error: argument of iszero is not a number"
