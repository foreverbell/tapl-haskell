module Runner (
  run
) where

import Control.DeepSeq (deepseq)
import Control.Monad (foldM)
import TAPL.Meow (Meow, meow)

import Base
import Context
import Evaluator (evaluate, unpackPattern)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf, typeOfPattern)

executeStatement :: Context -> Statement -> Meow Context
executeStatement ctx (BindType name ty) = do
  meow $ "[Type] " ++ pprintType ctx ty
  return $ addBinding ctx name (BindTypeAlias ty)

executeStatement ctx (BindLet pat t) = do
  let ty = typeOf ctx t
  let tyPat = ty `deepseq` typeOfPattern ctx ty pat
  let val = tyPat `deepseq` evaluate ctx t
  foldM add ctx (reverse $ zip (unpackPattern val pat) tyPat)
    where
      add ctx ((name, t), (_, ty)) = do
        let val = evaluate ctx t
        meow $ "[Variable] " ++ name ++ " = " ++ pprint ctx val ++ " : " ++ pprintType ctx ty
        return $ addBinding ctx name (BindTermAlias val ty)

executeStatement ctx (Eval t) = do
  let ty = typeOf ctx t
  let val = ty `deepseq` evaluate ctx t
  meow $ pprint ctx val ++ " : " ++ pprintType ctx ty
  return ctx

run :: Context -> String -> Meow Context
run ctx str = statements `deepseq` foldM executeStatement ctx statements
  where
    statements = parseTree ctx str
