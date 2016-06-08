module Context (
  makeEmptyContext
, nameToIndex
, indexToBinding
, addBinding
, addName
, dropBindings
, dropOneBinding
, pickFreshName
) where

import           Data.List (findIndex)
import qualified Data.HashSet as S

import           Base

-- | We are mixing term and type bindings in the same context, be careful when using it!

makeEmptyContext :: Context
makeEmptyContext = Context []

nameToIndex :: Context -> String -> Int
nameToIndex (Context ctx) name = case findIndex (\(var, _) -> var == name) ctx of
  Just index -> index
  Nothing -> error $ if isVariable
               then "context error: variable " ++ name ++ " is unbound"
               else "context error: type " ++ name ++ " is not in scope"
  where isVariable = let c = head name in c >= 'a' && c <= 'z'

indexToBinding :: Context -> Int -> (String, Binding)
indexToBinding (Context ctx) index = ctx !! index

addBinding :: Context -> String -> Binding -> Context
addBinding (Context ctx) name binding = Context ((name, binding) : ctx)

addName :: Context -> String -> Context
addName ctx name = addBinding ctx name BindDeBruijn

dropBindings :: Context -> Int -> Context
dropBindings (Context ctx) n = Context (drop n ctx)

dropOneBinding :: Context -> Context
dropOneBinding ctx = dropBindings ctx 1

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx "_" = (addName ctx "_", "_")
pickFreshName (Context ctx) name = (addName (Context ctx) freshName, freshName)
  where
    ctx' = S.fromList $ map fst ctx
    freshName = if name `S.member` ctx' then go 1 else name
    go index = let fresh = name ++ "_" ++ show index
                in if fresh `S.member` ctx' then go (index + 1) else fresh
