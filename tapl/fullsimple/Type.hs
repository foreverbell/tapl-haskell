module Type (
  typeEqual
, typeOf
, simplifyType
, evaluateType
) where

import Base
import Context

typeMap :: (Int -> TermType) -> TermType -> TermType
typeMap onvar ty = go ty
  where
    go TypeBool = TypeBool
    go TypeNat = TypeNat
    go TypeUnit = TypeUnit
    go (TypeArrow ty1 ty2) = TypeArrow (go ty1) (go ty2)
    go (TypeVar var) = onvar var

typeShift :: TermType -> Int -> TermType
typeShift ty delta = typeMap (\var -> TypeVar (var + delta)) ty

getBindingType :: Context -> Int -> TermType
getBindingType ctx index = typeShift ty (index + 1) -- Shift all type variables to meet the current context.
  where
    ty = case snd (indexToBinding ctx index) of
           BindTypeAlias tyalias -> tyalias
           BindVar tyvar -> tyvar
           BindTermAlias _ (Just ty) -> ty
           _ -> undefined

-- | Simplify type to the form that the outer most is not TypeVar.
simplifyType :: Context -> TermType -> TermType
simplifyType ctx (TypeVar var) = simplifyType ctx (getBindingType ctx var)
simplifyType _ ty = ty

-- | Evaluate type to eliminate all TypeVars.
evaluateType :: Context -> TermType -> TermType
evaluateType ctx (TypeVar var) = evaluateType ctx (getBindingType ctx var)
evaluateType ctx (TypeArrow ty1 ty2) = TypeArrow (evaluateType ctx ty1) (evaluateType ctx ty2)
evaluateType _ ty = ty

typeEqual :: Context -> TermType -> TermType -> Bool
typeEqual ctx ty1 ty2 = case (ty1, ty2) of
  (TypeBool, TypeBool) -> True
  (TypeNat, TypeNat) -> True
  (TypeUnit, TypeUnit) -> True
  (TypeArrow tya1 tya2, TypeArrow tyb1 tyb2) -> typeEqual ctx tya1 tyb1 && typeEqual ctx tya2 tyb2
  (TypeVar i, TypeVar j) -> i == j
  (TypeVar _, _) -> typeEqual ctx (simplifyType ctx ty1) ty2
  (_, TypeVar _) -> typeEqual ctx ty1 (simplifyType ctx ty2)
  _ -> False

typeOf :: Context -> Term -> TermType

typeOf ctx (TermIfThenElse t1 t2 t3) =
  if typeEqual ctx (typeOf ctx t1) TypeBool
    then let ty2 = typeOf ctx t2
             ty3 = typeOf ctx t3
          in if typeEqual ctx ty2 ty3
               then ty2
               else error "type error: arms of conditional have different types"
    else error "type error: guard of conditional not a boolean"

typeOf _ TermTrue = TypeBool

typeOf _ TermFalse = TypeBool

typeOf _ TermZero = TypeNat

typeOf ctx (TermSucc t) =
  if typeEqual ctx (typeOf ctx t) TypeNat
    then TypeNat
    else error "type error: argument of succ is not a number"

typeOf ctx (TermPred t) =
  if typeEqual ctx (typeOf ctx t) TypeNat
    then TypeNat
    else error "type error: argument of pred is not a number"

typeOf ctx (TermIsZero t) =
  if typeEqual ctx (typeOf ctx t) TypeNat
    then TypeBool
    else error "type error: argument of iszero is not a number"

typeOf _ TermUnit = TypeUnit

typeOf ctx (TermLet var t1 t2) = typeShift (typeOf ctx' t2) (-1)
  where
    ty1 = typeOf ctx t1
    ctx' = addBinding ctx var (BindVar ty1)

typeOf ctx (TermVar var) = getBindingType ctx var

typeOf ctx (TermAbs var ty t) = TypeArrow ty (typeShift (typeOf ctx' t) (-1))
  where
    ctx' = addBinding ctx var (BindVar ty)

typeOf ctx (TermApp t1 t2) = case ty1 of
  TypeArrow ty3 ty4 ->
    if typeEqual ctx ty3 ty2
      then ty4
      else error "type error: parameter type mismatch"
  _ -> error "type error: arrow type expected"
  where
    ty1 = simplifyType ctx (typeOf ctx t1)
    ty2 = typeOf ctx t2

typeOf ctx (TermAscribe t ty) =
  if typeEqual ctx ty ty0
    then ty
    else error "type error: body of as-term does not have the expected type"
  where
    ty0 = typeOf ctx t
