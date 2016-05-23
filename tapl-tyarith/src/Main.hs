module Main where

import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)

import Parser (parseTree)
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

run :: String -> IO ()
run str = do
  let val = do
        term <- parseTree str
        ty <- typeCheck term
        val <- eval term
        return (val, ty)
  case val of
    Left err -> putStrLn err
    Right val -> putStrLn $ pprint val

usage = putStrLn "usage: tapl-tyarith <infile>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "tapl-tyarith> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
