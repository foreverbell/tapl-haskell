module Main where

import Control.Monad (forever)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import TAPL.Meow (exec_)
import Text.Printf (printf)

import Runner (run)

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "simplesub> "
        hFlush stdout
        mapM_ putStrLn =<< exec_ . run =<< getLine
    [sourceFile] -> mapM_ putStrLn =<< exec_ . run =<< readFile sourceFile
    _ -> usage
