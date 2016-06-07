module Main where

import Control.Monad (forever, foldM, void)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Base
import Context
import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf, evaluateType)

executeStatement :: Context -> Statement -> IO Context
executeStatement ctx (Bind name (BindTypeAlias ty)) = do
  let ty' = evaluateType ctx ty
  putStrLn $ pprintType ctx ty'
  return $ addBinding ctx name (BindTypeAlias ty')

executeStatement ctx (Bind name (BindTermAlias t _)) = do
  let ty = typeOf ctx t
  let val = evaluate ctx t
  putStrLn $ pprint ctx val ++ " : " ++ pprintType ctx ty
  return $ addBinding ctx name (BindTermAlias val (Just ty))

executeStatement _ (Bind _ _) = undefined

executeStatement ctx (Eval t) = do
  let ty = typeOf ctx t
  let val = evaluate ctx t
  putStrLn $ pprint ctx val ++ " : " ++ pprintType ctx ty
  return ctx

run :: String -> IO ()
run str = void $ foldM executeStatement makeEmptyContext (parseTree str)

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "fullsimple> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
