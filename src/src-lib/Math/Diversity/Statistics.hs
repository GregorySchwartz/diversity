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
-- zero indexing
median :: [Double] -> Maybe Double
median xs | null xs  = Nothing
          | odd  len = Just $ xs !! mid
          | even len = Just meanMedian
  where
    len        = length xs
    mid        = len `div` 2
    meanMedian = (xs !! (mid - 1) + xs !! mid) / 2

-- | Get the median and median absolute deviation (MAD) of a list of numbers
medmad :: [Double] -> (Double, Double)
medmad xs = (sampleMedian, mad)
  where
    mad          = fromJust
                 . median
                 . sort
                 . map (\x -> abs (x - sampleMedian))
                 $ xs
    sampleMedian = fromJust . median . sort $ xs
