-- GenerateDiversity module.
-- By G.W. Schwartz
--
-- Collection of functions for the collection of fragments for the
-- diversity calculations.

module GenerateDiversity where

-- Built in
import qualified Data.Map as M
import Data.List
import Data.Fasta.String

-- Local
import Types

-- | Generates fragment list from string of "win" length. This version
-- differs from normal as it takes a tuple with the position as the first
-- entry
fragmentPos :: Int -> [(Position, String)] -> [(Position, String)]
fragmentPos win xs | length xs < win = []
                   | otherwise       = combine (take win xs)
                                     : fragmentPos win (tail xs)
  where
    combine = foldl1' (\(x, ys) (_, y) -> (x, ys ++ y))

-- | Generate the PositionMap from a list of FastaSequences
generatePositionMap :: Window -> [FastaSequence] -> PositionMap
generatePositionMap win = M.fromListWith (++) . posSeqList
  where
    posSeqList    = map toList . concatMap (\x -> fragmentPos win
                                           . map (\(p, f) -> (p, [f]))
                                           . filter (\(_, f) -> noGaps f)
                                           . zip [1..]
                                           . fastaSeq
                                           $ x)
    toList (x, y) = (x, [y])
    noGaps y = y /= '-' && y /= '.'
