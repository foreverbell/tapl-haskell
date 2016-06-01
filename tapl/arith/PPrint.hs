module PPrint (
  pprint
) where

import Base

pprint :: Term -> String
pprint TermTrue = "true"
pprint TermFalse = "false"
pprint t = show (go t)
  where
    go :: Term -> Int
    go TermZero = 0
    go (TermSucc nv) = succ $ go nv
    go _ = undefined
