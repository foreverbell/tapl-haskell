module Main where

import Control.Monad (forever, foldM)
import System.Environment (getProgName, getArgs)
import System.IO (stdout, hFlush)
import Text.Printf (printf)

import Base
import Context
import Parser (parseTree)
import PPrint (pprintType)
import Type (typeOf, evaluateType)

run :: String -> IO ()
run str = do
  let commands = parseTree str
  let foo ctx (Bind name (BindTypeAlias ty)) = do
        let ty' = evaluateType ctx ty
        putStrLn $ pprintType ctx ty'
        return $ addBinding ctx name (BindTypeAlias ty)
  foldM foo makeEmptyContext commands
  return ()

usage :: IO ()
usage = printf "usage: %s <infile>\n" =<< getProgName

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      forever $ do
        putStr "fullsimple> "
        hFlush stdout
        run =<< getLine
    [sourceFile] -> run =<< readFile sourceFile
    _ -> usage
