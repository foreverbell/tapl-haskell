module Main where

import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import DeBruijn (deBruijn)
import Parser (parseTree)
import TypeChecker (typeCheck)

run :: String -> IO ()
run str = do
  let parsed = parseTree str
  let term = deBruijn parsed
  let ttype = typeCheck term
  putStrLn $ show term ++ " : " ++ show ttype

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
