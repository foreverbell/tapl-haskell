module Type (
  typeOf
) where

import Context
import Base

typeOf :: Term -> TermType
typeOf term = go makeEmptyContext term

go :: Context -> Term -> TermType

{- T-True -}
go _ TermTrue = TypeBool

{- T-False -}
go _ TermFalse = TypeBool

{- T-If -}
go ctx (TermIfThenElse t t1 t2) = case go ctx t of
  TypeBool -> if ty1 == ty2 
                then ty1
                else error "type error: arms of conditional have different types"
  _ -> error "type error: guard of conditional not a boolean"
  where
    ty1 = go ctx t1
    ty2 = go ctx t2

{- T-Var -}
go ctx (TermVar var) = snd (indexToName ctx var)

{- T-Abs -}
go ctx (TermAbs name vty t) = TypeArrow vty tty
  where
    ctx' = addName ctx name vty
    tty = go ctx' t

{- T-App -}
go ctx (TermApp t1 t2) = case ty1 of
  TypeArrow ty11 ty12 -> if ty2 == ty11 then ty12 else error "type error: parameter type mismatch"
  _ -> error "type error: arrow type expected"
  where
    ty1 = go ctx t1
    ty2 = go ctx t2
