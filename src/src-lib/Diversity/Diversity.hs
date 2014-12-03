-- Diversity module.
-- By G.W. Schwartz
--
-- Collection of functions pertaining to finding the diversity of samples.

module Diversity.Diversity where

-- Built-in
import Data.List
import Data.Ratio
import Numeric.SpecFunctions (choose)

-- Takes two strings, returns Hamming distance
hamming :: String -> String -> Int
hamming xs ys = length $ filter not $ zipWith (==) xs ys

-- Returns the diversity of a list of things
diversity :: (Ord b) => Double -> [b] -> Double
diversity order sample
    | length sample == 0 = 0
    | order == 1         = exp . h $ speciesList
    | otherwise          = (sum . map ((** order) . p_i) $ speciesList) ** pow
  where
    pow          = 1 / (1 - order)
    h            = negate . sum . map (\x -> (p_i x) * (log (p_i x)))
    p_i x        = ((fromIntegral . length $ x) :: Double) /
                   ((fromIntegral . length $ sample) :: Double)
    speciesList  = group . sort $ sample

specialBinomial :: Bool -> Integer -> Integer -> Integer -> Double
specialBinomial False n_total g n = fromRational
    $ product [(n_total - g - n + 1)..(n_total - g)]
    % product [(n_total - n + 1)..n_total]
specialBinomial True n_total g n = choose
                                   (fromIntegral n_total - fromIntegral g)
                                   (fromIntegral n)

-- Returns the rarefaction curve for each position in a list
rarefactionCurve :: (Eq a, Ord a) => Bool -> [a] -> [Double]
rarefactionCurve fastBin xs = map rarefact [1..n_total]
  where
    rarefact n
        | n == 0       = 0
        | n == 1       = 1
        | n == n_total = k
        | otherwise    = k - inner n
    inner n = ( \x -> if fastBin
                        then x / choose (fromIntegral n_total) (fromIntegral n)
                        else x )
            . sum
            . map (\g -> specialBinomial fastBin n_total g n)
            $ grouped
    n_total = genericLength xs
    k       = genericLength grouped
    grouped = map genericLength . group . sort $ xs

-- Calculates the percent of the curve that is above 95% of height of the curve
rarefactionViable :: [Double] -> Double
rarefactionViable xs = (genericLength valid / genericLength xs) * 100
  where
    valid = dropWhile (< (0.95 * last xs)) xs

