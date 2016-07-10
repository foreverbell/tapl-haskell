module Type (
  typeOf
) where

import           Base
import           Context

import           Data.List (find, sortBy)
import           Data.Function (on)
import qualified Data.HashSet as S

-- | Check if ty1 is subtype of ty2, i.e. ty1 <: ty2.
isSubtype :: TermType -> TermType -> Bool

{- SA-Top -}
isSubtype _ TypeTop = True

{- SA-Bool -}
isSubtype TypeBool TypeBool = True

{- SA-Rcd -}
isSubtype (TypeRecord fields1) (TypeRecord fields2) =
  flip all fields2 $ \(f2, ty2) ->
    case find (\(f1, _) -> f1 == f2) fields1 of
      Just (_, ty1) -> isSubtype ty1 ty2
      Nothing -> False

{- SA-Arrow -}
isSubtype (TypeArrow tyS1 tyS2) (TypeArrow tyT1 tyT2) =
  isSubtype tyT1 tyS1 && isSubtype tyT2 tyS2

isSubtype _ _ = False

sortFields :: [(String, TermType)] -> [(String, TermType)]
sortFields = sortBy (compare `on` fst)

-- | The type join (intersection).
join :: TermType -> TermType -> TermType
join TypeBool TypeBool = TypeBool
join (TypeArrow tyS1 tyS2) (TypeArrow tyT1 tyT2) =
  TypeArrow (meet tyS1 tyT1) (join tyS2 tyT2)
join (TypeRecord fields1) (TypeRecord fields2) = TypeRecord $ intersect (sortFields fields1) (sortFields fields2)
  where
    intersect [] _ = []
    intersect _ [] = []
    intersect v1@((f1, ty1) : r1) v2@((f2, ty2) : r2) = case compare f1 f2 of
      LT -> intersect r1 v2
      GT -> intersect v1 r2
      EQ -> (f1, join ty1 ty2) : intersect r1 r2
join _ _ = TypeTop

-- | The type meet (union).
meet :: TermType -> TermType -> TermType
meet TypeTop ty = ty
meet ty TypeTop = ty
meet TypeBool TypeBool = TypeBool
meet (TypeArrow tyS1 tyS2) (TypeArrow tyT1 tyT2) = 
  TypeArrow (join tyS1 tyT1) (meet tyS2 tyT2)
meet (TypeRecord fields1) (TypeRecord fields2) = TypeRecord $ union (sortFields fields1) (sortFields fields2)
  where
    union [] x = x
    union x [] = x
    union v1@((f1, ty1) : r1) v2@((f2, ty2) : r2) = case compare f1 f2 of
      LT -> (f1, ty1) : union r1 v2
      GT -> (f2, ty2) : union v1 r2
      EQ -> (f1, meet ty1 ty2) : union r1 r2
meet _ _ = error "can't meet two types"

-- | Algorithmic typing.
typeOf :: Term -> TermType
typeOf t = go makeEmptyContext t

go :: Context -> Term -> TermType

{- TA-IfThenElse -}
go ctx (TermIfThenElse t t1 t2) = case go ctx t of
  TypeBool -> join ty1 ty2
  _ -> error "type error: guard of conditional not a boolean"
  where
    ty1 = go ctx t1
    ty2 = go ctx t2

{- TA-True -}
go _ TermTrue = TypeBool

{- TA-False -}
go _ TermFalse = TypeBool

{- TA-Rcd -}
go ctx (TermRecord fields) =
  if unique
    then TypeRecord (map (\(f, t) -> (f, go ctx t)) fields)
    else error "type error: record contains duplicate field"
  where
    unique = S.size (S.fromList (map fst fields)) == length fields

{- TA-Proj -}
go ctx (TermProj t field) = case go ctx t of
  TypeRecord fields -> case find (\(f, _) -> f == field) fields of
                         Just (_, ty) -> ty
                         Nothing -> error $ "type error: field " ++ field ++ " not found"
  _ -> error "type error: expected record type"

go ctx (TermAscribe t ty) = if isSubtype (go ctx t) ty
  then ty
  else error "type error: can't do ascribe subsumption"

{- TA-Var -}
go ctx (TermVar var) = snd $ indexToName ctx var

{- TA-Abs -}
go ctx (TermAbs var ty t) = TypeArrow ty (go ctx' t)
  where
    ctx' = addName ctx var ty

{- TA-App -}
go ctx (TermApp t1 t2) = case ty1 of
  TypeArrow ty3 ty4 -> 
    if isSubtype ty2 ty3
      then ty4
      else error "type error: parameter type mismatch"
  _ -> error "type error: arrow type expected"
  where
    ty1 = go ctx t1
    ty2 = go ctx t2
