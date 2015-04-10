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
import qualified Data.Set as Set
import Numeric.SpecFunctions (choose)
import Data.Function (on)

-- Local
import Math.Diversity.RandomSampling

-- | Takes two strings, returns Hamming distance
hamming :: String -> String -> Int
hamming xs ys = length $ filter not $ zipWith (==) xs ys

-- | Fast product division
productDivision :: Double -> [Integer] -> [Integer] -> Double
productDivision acc [] []     = acc
productDivision acc [] (y:ys) = (acc / fromInteger y)
                              * productDivision acc [] ys
productDivision acc (x:xs) [] = acc * fromInteger x * productDivision acc xs []
productDivision acc (x:xs) (y:ys)
    | x == y    = productDivision acc xs ys
    | otherwise = (fromInteger x / fromInteger y) * productDivision acc xs ys

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
specialBinomial False n_total g n = productDivision 1 num den
  where
    num = [(n_total - g - n + 1)..(n_total - g)]
    den = [(n_total - n + 1)..n_total]
specialBinomial True n_total g n = choose
                                   (fromIntegral n_total - fromIntegral g)
                                   (fromIntegral n)

-- | Returns the rarefaction curve for each position in a list
rarefactionCurve :: (Eq a, Ord a)
                 => Bool
                 -> Int
                 -> Integer
                 -> Integer
                 -> [a]
                 -> IO [(Int, (Double, Double))]
rarefactionCurve !fastBin !runs !start !interval !xs =
        mapM rarefact $ [start,(start + interval)..(n_total - 1)] ++ [n_total]
  where
    rarefact !n
        | n == 0       = return (fromIntegral n, (0, 0))
        | n == 1       = return (fromIntegral n, (1, 0))
        | n == n_total = return (fromIntegral n, (k, 0))
        | runs == 0    = return (fromIntegral n, (k - inner n, 0))
        | otherwise    = do
            statTuple <-
                subsampleES runs (fromIntegral n_total) (fromIntegral n) xs
            return (fromIntegral n, statTuple)
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
                       -> IO [(Int, (Double, Double))]
rarefactionSampleCurve !fastBin !start !interval !ls =
    mapM rarefact $ [start,(start + interval)..(t_total - 1)] ++ [t_total]
  where
    rarefact !t
        | t == 0       = return (t, (0, 0))
        | t == t_total = return (t, (richness, 0))
        | otherwise    = return (t, (richness - inner t, 0))
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
rarefactionViable !xs = (genericLength valid / genericLength xs) * 100
  where
    valid = dropWhile (< (0.95 * last xs)) xs
