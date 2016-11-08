module Runner (
  run
) where

import Control.DeepSeq (deepseq)
import TAPL.Meow (Meow, meow)

import Base
import Evaluator (evaluate)
import Parser (parseTree)
import PPrint (pprint, pprintType)
import Type (typeOf)

run :: String -> Meow ()
run str = do
  let t = parseTree str
  let ty = t `deepseq` typeOf t
  case ty of
    TypeTop -> meow "warning: typechecker detects Top type"
    _ -> return ()
  let val = ty `deepseq` evaluate t
  meow $ pprint val ++ " : " ++ pprintType ty
