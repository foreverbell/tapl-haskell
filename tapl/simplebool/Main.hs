module Main where

import Control.DeepSeq (deepseq)
import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf)

run :: String -> IO ()
run str = do
  let t = parseTree str
  let ty = t `deepseq` typeOf t
  let val = ty `deepseq` evaluate t
  putStrLn $ pprint val ++ " : " ++ pprintType ty

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "simplebool> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
