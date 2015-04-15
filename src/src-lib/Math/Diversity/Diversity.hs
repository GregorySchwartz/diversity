-- Diversity module.
-- By G.W. Schwartz
--
{- | Collection of functions pertaining to finding the diversity of samples.
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.Diversity ( hamming
                                , diversity
                                , diversityOfMap
                                , rarefactionCurve
                                , rarefactionSampleCurve
                                , rarefactionViable ) where

-- Built-in
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Numeric.SpecFunctions (choose)
import Data.Function (on)

-- Local
import Math.Diversity.RandomSampling
import Math.Diversity.Types

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
    | order == 1         = exp . h . speciesList $ sample
    | otherwise          = ( Map.foldl' (+) 0
                           . Map.map ((** order) . p_i)
                           . speciesList
                           $ sample )
                        ** pow
  where
    pow          = 1 / (1 - order)
    h            = negate
                 . Map.foldl' (+) 0
                 . Map.map (\x -> p_i x * log (p_i x))
    p_i x        = (x :: Double) /
                   ((Map.foldl' (+) 0 . speciesList $ sample) :: Double)
    speciesList  = Map.fromListWith (+) . map (\x -> (x, 1))

-- | Returns the diversity of a map of things
diversityOfMap :: Double -> Map.Map (Sample, Fragment) Int -> Double
diversityOfMap order sample
    | Map.null sample    = 0
    | order == 1         = exp . h $ sample
    | otherwise          = ( Map.foldl' (+) 0
                           . Map.map ((** order) . p_i)
                           $ sample )
                        ** pow
  where
    pow          = 1 / (1 - order)
    h            = negate
                 . Map.foldl' (+) 0
                 . Map.map ( \x -> p_i (fromIntegral x)
                           * log (p_i (fromIntegral x)))
    p_i x        = (fromIntegral x :: Double) /
                   (fromIntegral (Map.foldl' (+) 0 sample) :: Double)

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
rarefactionCurve :: Bool
                 -> Int
                 -> Integer
                 -> Integer
                 -> Map.Map (Sample, Fragment) Int
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
            statTuple <- subsampleES
                         runs
                         (fromIntegral n_total)
                         (fromIntegral n)
                       . concatMap snd
                       . Map.toAscList
                       . Map.mapWithKey (\(s, f) x -> replicate x f)
                       $ xs
            return (fromIntegral n, statTuple)
    inner n = ( \x -> if fastBin
                        then x / choose (fromIntegral n_total) (fromIntegral n)
                        else x )
            . sum
            . map (\g -> specialBinomial fastBin n_total (fromIntegral g) n)
            $ grouped
    n_total = fromIntegral . Map.foldl' (+) 0 $ xs
    k       = fromIntegral . Map.size $ xs
    grouped = Map.elems xs

-- | Each sample has a collection of species, return a list of these maps
getSampleContents :: Map.Map (Sample, Fragment) Int -> [Set.Set Fragment]
getSampleContents = Map.elems
                  . Map.fromListWith Set.union
                  . map (\(x, y) -> (x, Set.singleton y))
                  . Map.keys

-- | Returns the rarefaction curve for each position in a list
rarefactionSampleCurve :: Bool
                       -> Int
                       -> Int
                       -> Map.Map (Sample, Fragment) Int
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
    speciesList = Map.keys . Map.mapKeys (\(x, y) -> y) $ ls
    t_total     = genericLength samples
    samples     = getSampleContents ls

-- | Calculates the percent of the curve that is above 95% of height of the
-- curve
rarefactionViable :: [Double] -> Double
rarefactionViable !xs = (genericLength valid / genericLength xs) * 100
  where
    valid = dropWhile (< (0.95 * last xs)) xs
