-- GenerateDiversity module.
-- By G.W. Schwartz
--
{- | Collection of functions for the collection of fragments for the
diversity calculations.
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.GenerateDiversity ( fragmentPos
                                        , generatePositionMap ) where

-- Built in
import qualified Data.Map as M
import Data.List
import Data.Fasta.String
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Math.Diversity.Types

-- | Get the sample ID of a sequence
getSample :: Int -> FastaSequence -> Sample
getSample x = (!! (x - 1)) . Split.splitOn "|" . fastaHeader

-- | Generates fragment list from string of "win" length. This version
-- differs from normal as it takes a tuple with the position as the first
-- entry
fragmentPos :: Bool -> Int -> [(Position, Fragment)] -> [(Position, Fragment)]
fragmentPos whole win xs | whole && null xs = error "Empty line in file!!"
                         | whole            = [combine xs]
                         | length xs < win  = []
                         | otherwise        = combine (take win xs)
                                            : fragmentPos whole win (tail xs)
  where
    combine = foldl1' (\(!x, !ys) (_, y) -> (x, ys Seq.>< y))

-- | Generate the PositionMap from a list of FastaSequences
generatePositionMap :: Bool
                    -> Int
                    -> Bool
                    -> Window
                    -> [FastaSequence]
                    -> PositionMap
generatePositionMap sample sampleField whole win =
    M.fromListWith (++) . posSeqList
  where
    posSeqList    = map toList . concatMap (\x -> map (\(!p, !f) -> (p, sampleIt sample x f))
                                           . fragmentPos whole win
                                           . map (\(!p, !f) -> (p, Seq.singleton f))
                                           . filter (\(_, !f) -> noGaps f)
                                           . zip [1..]
                                           . fastaSeq
                                           $ x)
    toList (x, y) = (x, [y])
    noGaps y = y /= '-' && y /= '.'
    sampleIt True s f = (getSample sampleField s, f)
    sampleIt False _ f = ("Sample", f)
