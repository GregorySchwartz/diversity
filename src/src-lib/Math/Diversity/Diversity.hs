-- Diversity module.
-- By G.W. Schwartz
--
{- | Collection of functions pertaining to finding the diversity of samples.
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.Diversity ( hamming
                                , diversity
                                , rarefactionCurve
                                , rarefactionSampleCurve
                                , rarefactionViable ) where

-- Built-in
import Data.List
import Data.Ratio
import qualified Data.Set as Set
import Numeric.SpecFunctions (choose)
import Data.Function (on)

-- | Takes two strings, returns Hamming distance
hamming :: String -> String -> Int
hamming xs ys = length $ filter not $ zipWith (==) xs ys

-- | Returns the diversity of a list of things
diversity :: (Ord b) => Double -> [b] -> Double
diversity order sample
    | null sample        = 0
    | order == 1         = exp . h $ speciesList
    | otherwise          = (sum . map ((** order) . p_i) $ speciesList) ** pow
  where
    pow          = 1 / (1 - order)
    h            = negate . sum . map (\x -> p_i x * log (p_i x))
    p_i x        = ((fromIntegral . length $ x) :: Double) /
                   ((fromIntegral . length $ sample) :: Double)
    speciesList  = group . sort $ sample

-- | Binomial for small or large numbers (slow but works for big numbers,
-- fast but works for small numbers)
specialBinomial :: Bool -> Integer -> Integer -> Integer -> Double
specialBinomial False n_total g n = fromRational
    $ product [(n_total - g - n + 1)..(n_total - g)]
    % product [(n_total - n + 1)..n_total]
specialBinomial True n_total g n = choose
                                   (fromIntegral n_total - fromIntegral g)
                                   (fromIntegral n)

-- | Returns the rarefaction curve for each position in a list
rarefactionCurve :: (Eq a, Ord a)
                 => Bool
                 -> Integer
                 -> Integer
                 -> [a]
                 -> [(Int, Double)]
rarefactionCurve fastBin start interval xs =
    map rarefact $ [start,(start + interval)..(n_total - 1)] ++ [n_total]
  where
    rarefact n
        | n == 0       = (fromIntegral n, 0)
        | n == 1       = (fromIntegral n, 1)
        | n == n_total = (fromIntegral n, k)
        | otherwise    = (fromIntegral n, k - inner n)
    inner n = ( \x -> if fastBin
                        then x / choose (fromIntegral n_total) (fromIntegral n)
                        else x )
            . sum
            . map (\g -> specialBinomial fastBin n_total g n)
            $ grouped
    n_total = genericLength xs
    k       = genericLength grouped
    grouped = map genericLength . group . sort $ xs

-- | Each sample has a collection of species, return a list of these maps
getSampleContents :: (Ord a, Ord b) => [(a, b)] -> [Set.Set b]
getSampleContents = map (Set.fromList . map snd)
                  . groupBy ((==) `on` fst)
                  . sortBy (compare `on` fst)

-- | Returns the rarefaction curve for each position in a list
rarefactionSampleCurve :: (Ord a, Ord b)
                       => Bool
                       -> Int
                       -> Int
                       -> [(a, b)]
                       -> [(Int, Double)]
rarefactionSampleCurve fastBin start interval ls =
    map rarefact $ [start,(start + interval)..(t_total - 1)] ++ [t_total]
  where
    rarefact t
        | t == 0       = (t, 0)
        | t == t_total = (t, richness)
        | otherwise    = (t, richness - inner t)
    inner t     = ( \x -> if fastBin
                            then x / choose t_total t
                            else x )
                . sum
                . map ( \s -> specialBinomial
                              fastBin
                              (fromIntegral t_total)
                              (numHave s samples)
                              (fromIntegral t) )
                $ speciesList
    numHave s   = sum . map (\x -> if Set.member s x then 1 else 0)
    richness    = genericLength speciesList
    speciesList = nub . map snd $ ls
    t_total     = genericLength samples
    samples     = getSampleContents ls

-- | Calculates the percent of the curve that is above 95% of height of the curve
rarefactionViable :: [Double] -> Double
rarefactionViable xs = (genericLength valid / genericLength xs) * 100
  where
    valid = dropWhile (< (0.95 * last xs)) xs
