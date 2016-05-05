module NFA ( 
  build
, accept
) where

import           Data.List (group, sort)
import           Data.Maybe (fromJust, isJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Type

itranslate :: SetItem -> TransitionRule
itranslate (ItemChar c) = TransChar c
itranslate (ItemRange c1 c2) = TransRange c1 c2

shift :: Int -> Seq.Seq [Edge a] -> Seq.Seq [Edge a]
shift n = fmap (fmap (\(i, a) -> (i + n, a)))

addEdge :: (Int, Edge a) -> Seq.Seq [Edge a] -> Seq.Seq [Edge a]
addEdge (u, e) g = Seq.adjust (e:) u g

emptyNode :: Seq.Seq [Edge a]
emptyNode = Seq.singleton [ ]

symbol :: TransitionRule -> NFA
symbol rule = NFA $ Seq.fromList [ [(1, rule)] , [] ]

union :: NFA -> NFA -> NFA
union nfa1 nfa2 = NFA $ foldr addEdge (mconcat [ emptyNode, shift 1 g1, shift (1+n1) g2, emptyNode ]) newEdges
  where
    (g1, n1) = unNFA nfa1
    (g2, n2) = unNFA nfa2
    newEdges = [ (0, (1, TransEpsilon))
               , (n1, (n1+n2+1, TransEpsilon))
               , (0, (n1+1, TransEpsilon))
               , (n1+n2, (n1+n2+1, TransEpsilon))
               ]

concatenation :: NFA -> NFA -> NFA
concatenation nfa1 nfa2 = NFA $ mconcat [ Seq.take (n-1) g1, shift (n-1) g2 ]
  where
    (g1, n) = unNFA nfa1
    (g2, _) = unNFA nfa2

kleeneStar :: NFA -> NFA
kleeneStar nfa = NFA $ foldr addEdge (mconcat [ emptyNode, shift 1 g, emptyNode ]) newEdges
  where
    (g, n) = unNFA nfa
    newEdges = [ (0, (n+1, TransEpsilon))
               , (0, (1, TransEpsilon))
               , (n, (n+1, TransEpsilon))
               , (n, (1, TransEpsilon))
               ]

kleenePlus :: NFA -> NFA
kleenePlus nfa = NFA $ foldr addEdge (mconcat [ emptyNode, shift 1 g, emptyNode ]) newEdges
  where
    (g, n) = unNFA nfa
    newEdges = [ (0, (1, TransEpsilon))
               , (n, (n+1, TransEpsilon))
               , (n, (1, TransEpsilon))
               ]

-- | Thompson's construction, see https://en.wikipedia.org/wiki/Thompson%27s_construction
build :: RE -> NFA
build REAnyChar = symbol TransAny
build (REChar c) = symbol (TransChar c)
build (REPositiveSet xs) = symbol (TransPositive (map itranslate xs))
build (RENegativeSet xs) = symbol (TransNegative (map itranslate xs))
build (REUnion re1 re2) = union (build re1) (build re2)
build (REConcatenation re1 re2) = concatenation (build re1) (build re2)
build (REKleenePlus sub) = kleenePlus (build sub)
build (REKleeneStar sub) = kleeneStar (build sub)

accept :: NFA -> String -> Bool
accept nfa x = go x (epsClosure [0])
  where
    (g, n) = unNFA nfa

    go :: String -> [Int] -> Bool
    go [] xs = (n-1) `elem` xs
    go (c:cs) xs = go cs (epsClosure (move xs c))

    nub' :: (Ord a, Eq a) => [a] -> [a]
    nub' = map head . group . sort

    move1 :: Edge TransitionRule -> Char -> Maybe Int
    move1 (u, t) c = if translate t c then Just u else Nothing
      where
        translate TransEpsilon = const False -- expecting this transition *comsuming* a character
        translate TransAny = const True
        translate (TransChar ch) = (==) ch
        translate (TransPositive rules) = \ch -> any (($ ch) . translate) rules
        translate (TransNegative rules) = \ch -> not $ any (($ ch) . translate) rules
        translate (TransRange c1 c2) = \ch -> ch >= c1 && ch <= c2
    
    move :: [Int] -> Char -> [Int]
    move xs c = nub' $ concat $ do
      x <- xs
      let ys = map fromJust $ filter isJust $ map (`move1` c) (Seq.index g x)
      return ys

    epsClosure :: [Int] -> [Int]
    epsClosure xs = Set.toList $ foldr expand Set.empty xs
      where
        expand :: Int -> Set.Set Int -> Set.Set Int
        expand x s 
          | Set.member x s = s
          | otherwise = let f (u, TransEpsilon) s = expand u s
                            f _ s = s
                         in foldr f (Set.insert x s) (Seq.index g x)
