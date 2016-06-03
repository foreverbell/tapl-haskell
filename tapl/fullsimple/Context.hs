module Context ( 
  makeEmpty
, nameToIndex
, indexToBinding
, addBinding
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

indexToBinding :: Context -> Int -> (String, Binding)
indexToBinding (Context ctx) index = ctx !! index

addBinding :: Context -> String -> Binding -> Context
addBinding (Context ctx) name binding = Context ((name, binding) : ctx)

pickFreshName :: Context -> String -> (Context, String)
pickFreshName (Context ctx) name = (addBinding (Context ctx) freshName DeBruijnBind, freshName)
  where
    ctx' = S.fromList $ map fst ctx
    freshName = if name `S.member` ctx' then go 1 else name
    go index = let fresh = name ++ "_" ++ show index
                in if fresh `S.member` ctx' then go (index + 1) else fresh
