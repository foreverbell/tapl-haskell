module PPrint (
  pprint
) where

import Base 

pprint :: (Term, TermType) -> String
pprint (t, ty) = x ++ " : " ++ y
  where
    x = case t of
          TermTrue -> "true"
          TermFalse -> "false"
          _ -> show $ go t
      where
        go :: Term -> Int
        go TermZero = 0
        go (TermSucc nv) = succ $ go nv
        go _ = undefined
    y = case ty of
          TypeNat -> "Nat"
          TypeBool -> "Bool"
