module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)

import Lexer (alexScanTokens)
import Parser (happyParseRE)
import Type

re :: String -> Either String RE
re str = do
  tokens <- alexScanTokens str
  happyParseRE tokens
  
main :: IO ()
main = forever $ do
  putStr "regex> "
  hFlush stdout
  l <- getLine
  putStrLn $ show $ re l
