module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)
import Lexer
import Parser

eval :: Exp -> Integer
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Int i) = i

parse :: String -> Either String Integer
parse l = do
  ts <- alexScanTokens l
  exp <- happyParseExp ts
  return $ eval exp

main :: IO ()
main = forever $ do
  putStr "arith-evaluator> "
  hFlush stdout
  l <- getLine
  case parse l of
    Left err -> putStrLn err
    Right v -> putStrLn $ show v
