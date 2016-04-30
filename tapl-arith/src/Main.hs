module Main where

import Control.Monad (forever)
import System.IO (stdout, hFlush)

import Lexer (alexScanTokens)
import Parser (happyParseTerms)
import Type

isNumericVal :: Term -> Bool
isNumericVal TermZero = True
isNumericVal (TermPred t) = isNumericVal t
isNumericVal (TermSucc t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TermTrue = True
isVal TermFalse = True
isVal t = isNumericVal t

-- | One step evaluation, return Nothing if there is no rule applies.
eval1 :: Term -> Maybe Term

{- E-IfTrue -}
eval1 (TermIfThenElse TermTrue t1 _) = Just t1

{- E-IfFalse -}
eval1 (TermIfThenElse TermFalse _ t2) = Just t2

{- E-If -}
eval1 (TermIfThenElse tc t1 t2) = do
  tc' <- eval1 tc
  return $ TermIfThenElse tc' t1 t2

{- E-Succ -}
eval1 (TermSucc t) = do
  t' <- eval1 t
  return $ TermSucc t'

{- E-PredZero -}
eval1 (TermPred TermZero) = Just TermZero

{- E-PredSucc -}
eval1 (TermPred (TermSucc nv))
  | isNumericVal nv = Just nv

{- E-Pred -}
eval1 (TermPred t) = do
  t' <- eval1 t
  return $ TermPred t'

{- E-IsZeroZero -}
eval1 (TermIsZero TermZero) = Just TermTrue

{- E-IsZeroSucc -}
eval1 (TermIsZero (TermSucc nv))
  | isNumericVal nv = Just TermFalse

{- E-IsZero -}
eval1 (TermIsZero t) = do
  t' <- eval1 t
  return $ TermIsZero t'

{- E-NoRule -}
eval1 _ = Nothing

evalFull :: Term -> Term
evalFull t = case eval1 t of
  Just t' -> evalFull t'
  Nothing -> t

doMagic :: String -> Either String Term
doMagic s = do
  tokens <- alexScanTokens s
  term <- happyParseTerms tokens
  let nf = evalFull term
  if isVal nf
    then return nf
    else Left "eval error, term is malformed"

pprint :: Term -> String
pprint TermTrue = "true"
pprint TermFalse = "false"
pprint t = show $ go t
  where
    go :: Term -> Int
    go TermZero = 0
    go (TermSucc nv) = succ $ go nv
    go _ = undefined

main :: IO ()
main = forever $ do
  putStr "tapl-arith> "
  hFlush stdout
  l <- getLine
  case doMagic l of
    Left err -> putStrLn err
    Right v -> putStrLn $ pprint v
