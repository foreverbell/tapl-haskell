module Main where

import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import           Parser (parseTree)
import qualified NFA as NFA
import qualified VM as VM

run :: String -> String -> IO ()
run regex text = do
  case parseTree regex of
    Left err -> putStrLn err
    Right ast -> do
      let program = VM.compile ast
      let nfa = NFA.build ast
      putStrLn "Program:"
      putStrLn $ unlines $ map (uncurry showLine) $ zip [1 :: Int .. ] (VM.pprint program)
      putStrLn "\n"
      putStrLn $ "VM result: " ++ show (VM.accept program text)
      putStrLn $ "NFA result: " ++ show (NFA.accept nfa text)
  where showLine l p = printf " % 3d " l ++ p

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

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
