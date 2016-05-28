module PPrint (
  pprint
) where

import Types

pprint :: (Term, Type) -> String
pprint (tm, ty) = x ++ " : " ++ y
  where
    x = case tm of
          TermTrue -> "true"
          TermFalse -> "false"
          _ -> show $ go tm
      where
        go :: Term -> Int
        go TermZero = 0
        go (TermSucc nv) = succ $ go nv
        go _ = undefined
    y = case ty of
          TypeNat -> "Nat"
          TypeBool -> "Bool"
