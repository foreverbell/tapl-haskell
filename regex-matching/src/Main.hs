module Main where

import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)

import Parser (parseTree)
import NFA (build, accept)
import Types

run :: String -> String -> IO ()
run regex text = do
  let val = do
        nfa <- build <$> parseTree regex
        return $ accept nfa text
  case val of
    Left err -> putStrLn err
    Right val -> print val

usage = putStrLn "usage: regex-matching <infile>"

prompt :: String -> IO String
prompt p = do
  putStr p
  hFlush stdout
  getLine

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        regex <- prompt "regex> "
        text <- prompt "text> "
        run regex text
    [sourceFile] -> do
       [regex, text] <- lines <$> readFile sourceFile
       run regex text
    _ -> usage
