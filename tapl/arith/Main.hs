module Main where

import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint)

run :: String -> IO ()
run str = do
  let term = parseTree str
  let val = term `seq` evaluate term
  putStrLn $ pprint val

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "arith> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
