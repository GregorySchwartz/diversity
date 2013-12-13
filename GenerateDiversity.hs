-- GenerateDiversity module.
-- By G.W. Schwartz
--
-- Collection of functions for the collection of fragments for the
-- diversity calculations.

module GenerateDiversity where

-- Built in
import qualified Data.Map as M
import Data.Ord
import Data.List

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Types
import Diversity

-- | Same as takeWhile, but also cuts off at a certain length and ignores
-- unsatisfied predicates
takeWhile' :: (a -> Bool) -> Int -> [a] -> [a]
takeWhile' _ _ [] = []
takeWhile' p n (x:xs)
    | p x && n > 0 = x : takeWhile' p (n - 1) xs
    | not (p x) && n > 0 = takeWhile' p n xs
    | otherwise     = []

-- | Generates fragment list from string of "win" length
fragment :: Int -> String -> [String]
fragment win xs | length xs < win = []
                | length (takeWhile' noGaps win xs) /= win = []
                | not . noGaps . head $ xs = fragment win (tail xs)
                | otherwise = takeWhile' noGaps win xs : fragment win (tail xs)
  where
    noGaps y = y /= '-' && y /= '.'

-- | Generate the PositionMap from a list of FastaSequences
generatePositionMap :: Int -> [FastaSequence] -> PositionMap
generatePositionMap win = M.fromListWith (++) . posSeqList
  where
    posSeqList    = map toList . concatMap (\x -> zip [1..] . fragment win
                                           . fastaSeq $ x)
    toList (x, y) = (x, [y])

-- | Generate DiversityMap from a PositionMap which contains the diversity
-- at each position.
generateDiversityMap :: Order -> PositionMap -> DiversityMap
generateDiversityMap order = M.map (diversity order)
