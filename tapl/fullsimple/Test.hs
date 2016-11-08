module Main where

import Control.Monad (forM_)
import TAPL.Meow (exec_)
import Test.QuickCheck
import Text.Printf (printf)

import Context (makeEmptyContext)
import Runner (run)

testCount :: Int
testCount = 4

main :: IO ()
main = do
  forM_ [1 .. testCount] $ \i -> do
    input <- readFile (printf "fullsimple/examples/ex%d.in" i)
    expected <- readFile (printf "fullsimple/examples/outputs/ex%d.out" i)
    output <- unlines <$> exec_ (run makeEmptyContext input)
    printf "[%d/%d] " i testCount
    quickCheck (expected == output)
