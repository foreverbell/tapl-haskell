module Main where

import Control.Monad (foldM, foldM_, void)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Base
import Context
import Evaluator (evaluate, evaluatePattern)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf, evaluateType)

executeStatement :: Context -> Statement -> IO Context
executeStatement ctx (BindType name ty) = do
  let ty' = evaluateType ctx ty
  putStrLn $ "[Type] " ++ pprintType ctx ty'
  return $ addBinding ctx name (BindTypeAlias ty')

executeStatement ctx (BindLet pat t) = do
  let tyDummy = typeOf ctx (TermLet pat t (TermUnit)) -- This is HACK to check pattern matching typechecks.
  let ty = tyDummy `seq` typeOf ctx t
  let val = ty `seq` evaluate ctx t
  foldM merge ctx (reverse $ evaluatePattern val pat)
    where
      merge ctx (name, t) = do
        let ty = typeOf ctx t
        let val = ty `seq` evaluate ctx t
        putStrLn $ "[Variable] " ++ name ++ " = " ++ pprint ctx val ++ " : " ++ pprintType ctx ty
        return $ addBinding ctx name (BindTermAlias val ty)

executeStatement ctx (Eval t) = do
  let ty = typeOf ctx t
  let val = ty `seq` evaluate ctx t
  putStrLn $ pprint ctx val ++ " : " ++ pprintType ctx ty
  return ctx

run :: Context -> String -> IO Context
run ctx str = foldM executeStatement ctx (parseTree ctx str)

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
