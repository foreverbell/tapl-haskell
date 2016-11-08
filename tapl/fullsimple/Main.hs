module Main where

import Control.Monad (foldM_)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import TAPL.Meow (exec, exec_)
import Text.Printf (printf)

import Context
import Runner (run)

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      foldM_ loop makeEmptyContext [1 .. ]
        where
          loop ctx _ = do
            putStr "fullsimple> "
            hFlush stdout
            (ctx, out) <- exec . run ctx =<< getLine
            mapM putStrLn out
            return ctx
    [sourceFile] -> mapM_ putStrLn =<< exec_ . run makeEmptyContext =<< readFile sourceFile
    _ -> usage
