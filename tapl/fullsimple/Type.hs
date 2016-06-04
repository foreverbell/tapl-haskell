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

typeSubstitute :: TermType -> Int -> TermType -> TermType
typeSubstitute ty index subty = typeMap (\var -> if var == index then subty else TypeVar var) ty

simplifyType :: Context -> TermType -> TermType
simplifyType ctx (TypeVar var) = simplifyType ctx (typeShift tyalias (var + 1))
  where BindTypeAlias tyalias = snd $ indexToBinding ctx var
simplifyType _ ty = ty

-- | FIXME: TEST ONLY
evaluateType :: Context -> TermType -> TermType
evaluateType ctx (TypeVar var) = evaluateType ctx (typeShift tyalias (var + 1))
  where BindTypeAlias tyalias = snd $ indexToBinding ctx var
evaluateType ctx (TypeArrow ty1 ty2) = TypeArrow (evaluateType ctx ty1) (evaluateType ctx ty2)
evaluateType _ ty = ty

typeEqual :: Context -> TermType -> TermType -> Bool
typeEqual = undefined

typeOf :: Context -> Term -> TermType
typeOf = undefined
