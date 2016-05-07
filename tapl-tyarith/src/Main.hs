module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)

import Lexer (alexScanTokens)
import Parser (happyParseTerms)
import Evaluator (eval)
import TypeChecker (typeCheck)
import Types

pprint :: (Term, Type) -> String
pprint (tm, ty) = x ++ " : " ++ y
  where
    x = case tm of
          TermTrue -> "true"
          TermFalse -> "false"
          otherwise -> show $ go tm
      where
        go :: Term -> Int
        go TermZero = 0
        go (TermSucc nv) = succ $ go nv
        go _ = undefined
    y = case ty of
          TypeNat -> "Nat"
          TypeBool -> "Bool"

main :: IO ()
main = forever $ do
  putStr "tapl-tyarith> "
  hFlush stdout
  l <- getLine
  let r = do tokens <- alexScanTokens l
             term <- happyParseTerms tokens
             ty <- typeCheck term 
             val <- eval term
             return (val, ty)
  case r of
    Left err -> putStrLn err
    Right v -> putStrLn $ pprint v
