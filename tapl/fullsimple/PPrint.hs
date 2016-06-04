module PPrint (
  pprint
, pprintType
) where

import Base
import Context (indexToBinding)

import Text.Printf (printf)

pprint = undefined

pprintType :: Context -> TermType -> String
pprintType ctx ty = pprintArrowType ctx ty

pprintArrowType :: Context -> TermType -> String
pprintArrowType ctx (TypeArrow ty1 ty2) = printf "%s->%s" (pprintAtomicType ctx ty1) (pprintArrowType ctx ty2)
pprintArrowType ctx ty = pprintAtomicType ctx ty

pprintAtomicType :: Context -> TermType -> String
pprintAtomicType _ TypeBool = "Bool"
pprintAtomicType _ TypeNat = "Nat"
pprintAtomicType _ TypeUnit = "Unit"
pprintAtomicType ctx (TypeVar var) = fst $ indexToBinding ctx var
pprintAtomicType ctx t = printf "(%s)" (pprintType ctx t)
