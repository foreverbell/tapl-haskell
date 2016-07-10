module Main where

import Control.DeepSeq (deepseq)
import Control.Monad (foldM, foldM_, void)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Base
import Context
import Evaluator (evaluate, unpackPattern)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf, typeOfPattern, evaluateType)

executeStatement :: Context -> Statement -> IO Context
executeStatement ctx (BindType name ty) = do
  let ty' = evaluateType ctx ty
  putStrLn $ "[Type] " ++ pprintType ctx ty'
  return $ addBinding ctx name (BindTypeAlias ty')

executeStatement ctx (BindLet pat t) = do
  let ty = typeOf ctx t
  let tyPat = ty `deepseq` typeOfPattern ctx ty pat
  let val = tyPat `deepseq` evaluate ctx t
  foldM add ctx (reverse $ zip (unpackPattern val pat) tyPat)
    where
      add ctx ((name, t), (_, ty)) = do
        let val = evaluate ctx t
        putStrLn $ "[Variable] " ++ name ++ " = " ++ pprint ctx val ++ " : " ++ pprintType ctx ty
        return $ addBinding ctx name (BindTermAlias val ty)

executeStatement ctx (Eval t) = do
  let ty = typeOf ctx t
  let val = ty `deepseq` evaluate ctx t
  putStrLn $ pprint ctx val ++ " : " ++ pprintType ctx ty
  return ctx

run :: Context -> String -> IO Context
run ctx str = statements `deepseq` foldM executeStatement ctx statements
  where statements = parseTree ctx str

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      foldM_ loop makeEmptyContext [1 .. ]
        where
          loop ctx _ = do
            putStr "fullsimple> "
            hFlush stdout
            run ctx =<< getLine
    [sourceFile] -> void . run makeEmptyContext =<< readFile sourceFile
    _ -> usage
