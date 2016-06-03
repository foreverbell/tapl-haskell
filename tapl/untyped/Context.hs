module Context ( 
  makeEmptyContext
, nameToIndex
, indexToName
, addName
, dropOneName
, pickFreshName
) where

import           Data.List (findIndex)
import qualified Data.HashSet as S

import           Base

makeEmptyContext :: Context
makeEmptyContext = Context []

nameToIndex :: Context -> String -> Int
nameToIndex (Context ctx) name = case findIndex (== name) ctx of
  Just index -> index
  Nothing -> error $ "context error: variable " ++ name ++ " is unbound"

indexToName :: Context -> Int -> String
indexToName (Context ctx) index = ctx !! index

addName :: Context -> String -> Context
addName (Context ctx) name = Context (name : ctx)

dropOneName :: Context -> Context
dropOneName (Context ctx) = Context (tail ctx)

pickFreshName :: Context -> String -> (Context, String)
pickFreshName (Context ctx) name = (addName (Context ctx) freshName, freshName)
  where
    ctx' = S.fromList ctx
    freshName = if name `S.member` ctx' then go 1 else name
    go index = let fresh = name ++ "_" ++ show index
                in if fresh `S.member` ctx' then go (index + 1) else fresh
