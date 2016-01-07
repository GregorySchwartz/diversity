-- Statistics module.
-- By Gregory W. Schwartz
--
{- | Collection of functions pertaining statistics needed in the library
-}

module Math.Diversity.Statistics ( median, medmad ) where

-- Built-in
import Data.List
import Data.Maybe

-- | Get the median, adapted from
-- http://rosettacode.org/wiki/Averages/Median#Haskell, but FIXED due to
-- zero indexing. Also made sure that it is sorted
median :: [Double] -> Maybe Double
median xs | null xs  = Nothing
          | odd  len = Just $ sorted !! mid
          | even len = Just meanMedian
  where
    len        = length sorted
    mid        = len `div` 2
    meanMedian = (sorted !! (mid - 1) + sorted !! mid) / 2
    sorted     = sort xs

-- | Get the median and median absolute deviation (MAD) of a list of numbers
medmad :: [Double] -> Maybe (Double, Double)
medmad [] = Nothing
medmad xs = Just (sampleMedian, mad)
  where
    mad          = fromJust
                 . median
                 . map (\x -> abs (x - sampleMedian))
                 $ xs
    sampleMedian = fromJust . median $ xs
