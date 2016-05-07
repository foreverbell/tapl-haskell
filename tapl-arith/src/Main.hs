module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)

import Lexer (alexScanTokens)
import Parser (happyParseTerms)
import Evaluator (eval)
import Types

pprint :: Term -> String
pprint TermTrue = "true"
pprint TermFalse = "false"
pprint t = show $ go t
  where
    go :: Term -> Int
    go TermZero = 0
    go (TermSucc nv) = succ $ go nv
    go _ = undefined

main :: IO ()
main = forever $ do
  putStr "tapl-arith> "
  hFlush stdout
  l <- getLine
  let r = do tokens <- alexScanTokens l
             term <- happyParseTerms tokens
             eval term
  case r of
    Left err -> putStrLn err
    Right v -> putStrLn $ pprint v
