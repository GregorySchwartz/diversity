-- GenerateDiversity module.
-- By G.W. Schwartz
--
{- | Collection of functions for the collection of fragments for the
diversity calculations.
-}

module Diversity.GenerateDiversity ( fragmentPos
                                   , generatePositionMap ) where

-- Built in
import qualified Data.Map as M
import Data.List
import Data.Fasta.String

-- Local
import Diversity.Types

-- | Generates fragment list from string of "win" length. This version
-- differs from normal as it takes a tuple with the position as the first
-- entry
fragmentPos :: Bool -> Int -> [(Position, String)] -> [(Position, String)]
fragmentPos whole win xs | whole && null xs = error "Empty line in file!!"
                         | whole            = combine xs : []
                         | length xs < win  = []
                         | otherwise        = combine (take win xs)
                                            : fragmentPos whole win (tail xs)
  where
    combine = foldl1' (\(x, ys) (_, y) -> (x, ys ++ y))

-- | Generate the PositionMap from a list of FastaSequences
generatePositionMap :: Bool -> Window -> [FastaSequence] -> PositionMap
generatePositionMap whole win = M.fromListWith (++) . posSeqList
  where
    posSeqList    = map toList . concatMap (\x -> fragmentPos whole win
                                           . map (\(p, f) -> (p, [f]))
                                           . filter (\(_, f) -> noGaps f)
                                           . zip [1..]
                                           . fastaSeq
                                           $ x)
    toList (x, y) = (x, [y])
    noGaps y = y /= '-' && y /= '.'
