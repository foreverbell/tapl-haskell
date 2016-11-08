module Runner (
  run
) where

import Control.DeepSeq (deepseq)
import TAPL.Meow (Meow, meow)

import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf)

run :: String -> Meow ()
run str = do
  let t = parseTree str
  let ty = t `deepseq` typeOf t
  let val = ty `deepseq` evaluate t
  meow $ pprint val ++ " : " ++ pprintType ty
