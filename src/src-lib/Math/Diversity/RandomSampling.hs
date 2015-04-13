-- RandomSampling module.
-- By Gregory W. Schwartz
--
{- | Collection of functions pertaining to getting random samples from
- a population
-}

module Math.Diversity.RandomSampling ( subsample
                                     , subsampleSpecies
                                     , subsampleES ) where

-- Built-in
import qualified Data.Set as Set

-- Cabal
import Control.Monad.Random
import System.Random.Shuffle

-- Local
import Math.Diversity.Statistics

-- | Gets a random sampling by getting either the requested amount either
-- directly or through the inverse (for speed)
subsample :: (Eq a, Ord a) => Int -> Int -> StdGen -> [a] -> [a]
subsample size n gen xs
    | n > size `div` 2 = drop (size - n) shuffled
    | otherwise = take n shuffled
  where
    shuffled  = shuffle' xs size gen

-- | Get the number of types of entities in a subsample
subsampleSpecies :: (Eq a, Ord a) => Int -> Int -> StdGen -> [a] -> Int
subsampleSpecies size n gen = Set.size . Set.fromList . subsample size n gen

-- | Repeat the sampling to get a median and MAD value for the runs for the
-- expected species counts
subsampleES :: (Eq a, Ord a) => Int -> Int -> Int -> [a] -> IO (Double, Double)
subsampleES runs size n xs = do
    allRuns <- mapM (const run) [1..runs]

    let statTuple = medmad . map fromIntegral $ allRuns

    return statTuple
  where
    run = do
        gen <- newStdGen
        return $ subsampleSpecies size n gen xs
