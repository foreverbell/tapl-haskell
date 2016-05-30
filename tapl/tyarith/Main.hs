module Main where

import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint)
import TypeChecker (typeCheck)

run :: String -> IO ()
run str = do
  let term = parseTree str
  let ttype = term `seq` typeCheck term
  let val = ttype `seq` evaluate term
  putStrLn $ pprint (val, ttype)

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "tyarith> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
