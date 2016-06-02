module Context ( 
  makeEmpty
, nameToIndex
, indexToName
, addName
, pickFreshName
) where

import           Data.List (findIndex)
import qualified Data.HashSet as S

import           Base

makeEmpty :: Context
makeEmpty = Context []

nameToIndex :: Context -> String -> Int
nameToIndex (Context ctx) name = case findIndex (\(var, _) -> var == name) ctx of
  Just index -> index
  Nothing -> error $ "context error: variable " ++ name ++ " is unbound"

indexToName :: Context -> Int -> (String, Binding)
indexToName (Context ctx) index = ctx !! index

addName :: Context -> String -> Binding -> Context
addName (Context ctx) name binding = Context ((name, binding) : ctx)

-- TODO: Simple Binding without type information?
pickFreshName :: Context -> String -> Binding -> (Context, String)
pickFreshName (Context ctx) name binding = (addName (Context ctx) freshName binding, freshName)
  where
    ctx' = S.fromList $ map fst ctx
    freshName = if name `S.member` ctx' then go 1 else name
    go index = let fresh = name ++ "_" ++ show index
                in if fresh `S.member` ctx' then go (index + 1) else fresh
