module Main where

import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)

import DeBruijn (deBruijn)
import Evaluator (eval)
import Parser (parseTree)
import PPrint (pprint)

run :: String -> IO ()
run str = do
  let val = do
        parsed <- parseTree str
        term <- deBruijn parsed
        val <- eval term
        return val
  case val of
    Left err -> putStrLn err
    Right val -> putStrLn $ pprint val

usage = putStrLn "usage: tapl-untyped <infile>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "tapl-untyped> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
