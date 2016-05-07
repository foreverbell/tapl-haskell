module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)

import Lexer (alexScanTokens)
import Parser (happyParseRE)
import NFA (build, accept)
import Types

getNFA :: String -> Either String NFA
getNFA str = do
  tokens <- alexScanTokens str
  re <- happyParseRE tokens
  return $ build re

main :: IO ()
main = forever $ do
  putStr "regex> "
  hFlush stdout
  nfa <- getNFA <$> getLine
  case nfa of
    Left err -> putStrLn err
    Right nfa -> do
      putStr "text> "
      hFlush stdout
      print =<< (accept nfa <$> getLine)
