{-# LANGUAGE TypeFamilies #-}

module DeBruijn ( 
  deBruijn
) where

import qualified Context as C
import           Types

deBruijn :: Term Raw -> Either String (Term DeBruijn)
deBruijn term = go C.empty term
  where
    go ctx (TermVar var) = TermVar <$> C.nameToIndex ctx var
    go ctx (TermAbs var term1) = TermAbs var <$> go (C.addName ctx var) term1
    go ctx (TermApp term1 term2) = TermApp <$> go ctx term1 <*> go ctx term2
