module VM (
  pprint
, compile
, accept
, Program
) where

import           Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import           Data.Vector ((!))

import           Base

data Instruction
  = InsMatch
  | InsAny
  | InsChar Char
  | InsPosSet [InsSetItem]
  | InsNegSet [InsSetItem]
  | InsSplit Int Int
  | InsJump Int
  deriving (Show)

data InsSetItem
  = InsItemRange Char Char
  | InsItemChar Char
  deriving (Show)

newtype Program = Program (V.Vector Instruction)
  deriving (Show)

isInsMatch :: Instruction -> Bool
isInsMatch InsMatch = True
isInsMatch _ = False

pprint :: Program -> [String]
pprint (Program p) = map (uncurry pprintIns) $ zip [1 .. ] (V.toList p)
  where
    pprintIns _ InsMatch = "match"
    pprintIns _ InsAny = "any"
    pprintIns _ (InsChar c) = "char " ++ [c]
    pprintIns _ (InsPosSet s) = "pos-set [" ++ pprintSet s ++ "]"
    pprintIns _ (InsNegSet s) = "neg-set [" ++ pprintSet s ++ "]"
    pprintIns l (InsSplit a b) = "split " ++ show (l + a) ++ " " ++ show (l + b)
    pprintIns l (InsJump d) = "jmp " ++ show (l + d)

    pprintSet = concat . map f
      where
        f (InsItemRange l r) = [l] ++ "-" ++ [r]
        f (InsItemChar c) = [c]

compile :: RE -> Program
compile = Program . V.fromList . (\re -> build re ++ [InsMatch])
  where
    toInsItem (ItemRange c1 c2) = InsItemRange c1 c2
    toInsItem (ItemChar c) = InsItemChar c

    build :: RE -> [Instruction]

    build REAnyChar = [InsAny]

    build (REChar c) = [InsChar c]

    build (REPositiveSet s) = [InsPosSet (map toInsItem s)]

    build (RENegativeSet s) = [InsNegSet (map toInsItem s)]

    build (REConcatenation re1 re2) = build re1 ++ build re2

    build (REUnion re1 re2) = [InsSplit 1 (2 + length ins1)] ++ ins1 ++ [InsJump (1 + length ins2)] ++ ins2
      where
        ins1 = build re1
        ins2 = build re2

    build (REKleenePlus re) = ins ++ [InsSplit 1 (- (length ins))]
      where
        ins = build re

    build (REKleeneStar re) = [InsSplit 1 (2 + l)] ++ ins ++ [InsJump (- (1 + l))]
      where
        ins = build re
        l = length ins

accept :: Program -> String -> Bool
accept (Program p) x = go x (closure [0])
  where
    go :: String -> [Int] -> Bool
    go [] xs = any (\i -> isInsMatch (p ! i)) xs
    go (c:cs) xs = go cs (closure (move xs c))

    move :: [Int] -> Char -> [Int]
    move xs ch = map fromJust $ filter isJust (map eat xs)
      where
        eat i = case p ! i of
                  InsMatch -> Nothing
                  InsAny -> Just (i + 1)
                  InsChar c -> if cond then Just (i + 1) else Nothing
                    where cond = c == ch
                  InsPosSet s -> if cond then Just (i + 1) else Nothing
                    where cond = any test s
                  InsNegSet s -> if cond then Just (i + 1) else Nothing
                    where cond = not $ any test s
                  _ -> undefined

        test (InsItemChar c) = ch == c
        test (InsItemRange l r) = ch >= l && ch <= r

    closure :: [Int] -> [Int]
    closure xs = nub' $ concat $ map expand xs
      where
        expand i = nub' $ case p ! i of
                            InsSplit a b -> expand (i + a) ++ expand (i + b)
                            InsJump d -> expand (i + d)
                            _ -> [i]
