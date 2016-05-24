module Context ( 
  empty
, nameToIndex
, addName
) where

import Data.List (findIndex)

import Types (Context (..))

empty :: Context
empty = Context []

nameToIndex :: Context -> String -> Either String Int
nameToIndex (Context ctx) name = case findIndex (== name) ctx of
  Just index -> Right index
  Nothing -> Left $ "identifier " ++ name ++ " is unbound"

addName :: Context -> String -> Context
addName (Context ctx) name = Context (name:ctx)
