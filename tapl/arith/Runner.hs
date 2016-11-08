module Runner (
  run
) where

import Control.DeepSeq (deepseq)
import TAPL.Meow (Meow, meow)

import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint)

run :: String -> Meow ()
run str = do
  let term = parseTree str
  let val = term `deepseq` evaluate term
  meow $ pprint val
