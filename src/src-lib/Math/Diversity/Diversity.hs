-- Diversity module.
-- By G.W. Schwartz
--
{- | Collection of functions pertaining to finding the diversity of samples.
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.Diversity ( hamming
                                , richness
                                , diversity
                                , diversityOfMap
                                , chao1
                                , chao2
                                , chao1Var
                                , chao2Var
                                , rarefactionCurve
                                , rarefactionSampleCurve
                                , rarefactionViable
                                , individualG
                                , sampleG
                                , minRarefaction ) where

-- Built-in
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Numeric.SpecFunctions (choose)

-- Cabal
import qualified Data.List.Ordered as LO

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
diversity :: (Ord a) => Double -> [a] -> Double
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

-- | Returns the diversity of a map of the species and how many times it
-- appears
diversityOfMap :: (Ord a) => Double -> Map.Map a Int -> Double
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
                 . Map.map ( \x -> p_i x * log (p_i x))
    p_i x        = fromIntegral x / fromIntegral (Map.foldl' (+) 0 sample)

-- | Returns the richness of the observed data
richness :: (Ord a, Ord b) => Map.Map (a, b) c -> Int
richness = Map.size . Map.mapKeys snd

-- | Returns the map of species with how many samples they appear in
overlapSampleMap :: (Ord a, Ord b) => Map.Map (a, b) Int -> Map.Map b Int
overlapSampleMap = Map.mapKeysWith (+) snd . Map.map (const 1)

-- | Returns the number of a that appear x times
abundanceFreq :: (Ord a) => Int -> Map.Map a Int -> Int
abundanceFreq x = Map.size . Map.filter (== x)

-- | Returns the number of b that appear in x number of a. Notice that this
-- function takes in a normal frequency map, as it converts it with
-- overlapSampleMap.
overlapFreq :: (Ord a, Ord b) => Int -> Map.Map (a, b) Int -> Int
overlapFreq x = Map.size . Map.filter (== x) . overlapSampleMap

-- | Returns the chao1 estimator of a map of the species and how many times
-- it appears
chao1 :: (Ord a) => Map.Map a Int -> Double
chao1 sample
    | f2 > 0    = (f1 ** 2) / (2 * f2)
    | otherwise = (f1 * (f1 - 1)) / (2 * (f2 + 1))
  where
    f1  = fromIntegral . abundanceFreq 1 $ sample
    f2  = fromIntegral . abundanceFreq 2 $ sample

-- | Returns the chao2 estimator of a map of the sample labeled species
-- (sample, species) and how many times it appears. This will calculate the
-- overlap for you, so if you don't have the number of times it appears it
-- does not matter, you can set it to 1 and get the same result as it's all
-- about overlapping samples.
chao2 :: (Ord a, Ord b) => Map.Map (a, b) Int -> Double
chao2 samples
    | q2 > 0    = ((t - 1) / t) * ((q1 ** 2) / (2 * q2))
    | otherwise = ((t - 1) / t) * ((q1 * (q1 - 1)) / (2 * (q2 + 1)))
  where
    q1  = fromIntegral . overlapFreq 1 $ samples
    q2  = fromIntegral . overlapFreq 2 $ samples
    -- Saves time so don't have to recalculate
    t   = fromIntegral . Map.size . Map.mapKeys fst $ samples

-- | Returns the chao1 estimator variance of a map of the species
-- and how many times each one appears.
chao1Var :: (Ord a) => Map.Map a Int -> Double
chao1Var sample
    | f2 > 0    = f2
                * ( ((1 / 2) * ((n - 1) / n) * ((f1 / f2) ** 2))
                  + ((((n - 1) / n) ** 2) * ((f1 / f2) ** 3))
                  + ((1 / 4) * (((n - 1) / n) ** 2) * ((f1 / f2) ** 4))
                  )
    | otherwise = (((n - 1) / n) * ((f1 * (f1 - 1)) / 2))
                + ((((n - 1) / n) ** 2) * ((f1 * (((2 * f1) - 1) ** 2)) / 4))
                + ((((n - 1) / n) ** 2) * ((f1 ** 4) / (4 * sest)))
  where
    sest = fromIntegral (Map.size sample) + chao1 sample
    f1  = fromIntegral . abundanceFreq 1 $ sample
    f2  = fromIntegral . abundanceFreq 2 $ sample
    -- Saves time so don't have to recalculate
    n    = fromIntegral . Map.foldl' (+) 0 $ sample

-- | Returns the chao2 estimator variance of a map of the sample labeled species
-- (sample, species) and how many times it appears.
chao2Var :: (Ord a, Ord b) => Map.Map (a, b) Int -> Double
chao2Var samples
    | q2 > 0    = q2
                * ( ((1 / 2) * ((t - 1) / t) * ((q1 / q2) ** 2))
                  + ((((t - 1) / t) ** 2) * ((q1 / q2) ** 3))
                  + ((1 / 4) * ((t - 1 / t) ** 2) * ((q1 / q2) ** 4))
                  )
    | otherwise = (((t - 1) / t) * ((q1 * (q1 - 1)) / 2))
                + ((((t - 1) / t) ** 2) * ((q1 * (((2 * q1) - 1) ** 2)) / 4))
                + ((((t - 1) / t) ** 2) * ((q1 ** 4) / (4 * sest)))
  where
    sest = fromIntegral (richness samples) + chao2 samples
    q1   = fromIntegral . overlapFreq 1 $ samples
    q2   = fromIntegral . overlapFreq 2 $ samples
    -- Saves time so don't have to recalculate
    t    = fromIntegral . Map.size . Map.mapKeys fst $ samples

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
                 -> Integer
                 -> Map.Map (Sample, Fragment) Int
                 -> IO [(Int, Maybe (Double, Double))]
rarefactionCurve !fastBin !runs !start !interval !end !sample =
        mapM rarefact
      . LO.nubSort
      $ n_total : [start,(start + interval)..finish]
  where
    rarefact !n
        | n == 0       = return (fromIntegral n, Just (0, 0))
        | n == 1       = return (fromIntegral n, Just (1, 0))
        | n == n_total = return (fromIntegral n, Just (k, 0))
        | n > n_total  = return (fromIntegral n, Just (estimation n, 0))
        | runs == 0    = return (fromIntegral n, Just (k - inner n, 0))
        | otherwise    = do  -- Empirical version
            statTuple <- subsampleES
                         runs
                         (fromIntegral n_total)
                         (fromIntegral n)
                       . concatMap snd
                       . Map.toAscList
                       . Map.mapWithKey (\(_, f) x -> replicate x f)
                       $ sample
            return (fromIntegral n, statTuple)
    inner n = ( \x -> if fastBin
                        then x / choose (fromIntegral n_total) (fromIntegral n)
                        else x )
            . sum
            . map (\g -> specialBinomial fastBin n_total (fromIntegral g) n)
            $ grouped
    -- Unreadable unless I break the 80 column rule
    estimation n = fromIntegral n_total + (chao1 sample * (1 - exp (((fromIntegral n - fromIntegral n_total) / (- fromIntegral n_total)) * (fromIntegral (abundanceFreq 1 sample) / chao1 sample))))
    finish       = if end == 0 then n_total else end
    n_total      = fromIntegral . Map.foldl' (+) 0 $ sample
    k            = fromIntegral . Map.size $ sample
    grouped      = Map.elems sample

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
                       -> Int
                       -> Map.Map (Sample, Fragment) Int
                       -> IO [(Int, Maybe (Double, Double))]
rarefactionSampleCurve !fastBin !start !interval !end !ls =
    mapM rarefact
  . LO.nubSort
  $ t_total : [start,(start + interval)..finish]
  where
    rarefact !t
        | t == 0       = return (t, Just (0, 0))
        | t == t_total = return (t, Just (sobs, 0))
        | t > t_total  = return (t, Just (estimation t, 0))
        | otherwise    = return (t, Just (sobs - inner t, 0))
    inner t      = ( \x -> if fastBin
                             then x / choose t_total t
                             else x )
                 . sum
                 . map ( \s -> specialBinomial
                               fastBin
                               (fromIntegral t_total)
                               (numHave s samples)
                               (fromIntegral t) )
                 $ speciesList
    -- Unreadable unless I break the 80 column rule
    estimation t = sobs + (chao2 ls * (1 - exp ((- (fromIntegral t - fromIntegral t_total) * fromIntegral (overlapFreq 1 ls)) / (fromIntegral (overlapFreq 1 ls) + (fromIntegral t_total * chao2 ls)))))
    finish       = if end == 0 then t_total else end
    numHave s    = genericLength . filter (Set.member s)
    sobs         = fromIntegral $ richness ls
    speciesList  = Map.keys . Map.mapKeys snd $ ls
    t_total      = genericLength samples
    samples      = getSampleContents ls

-- | Calculates the percent of the curve that is above 95% of height of the
-- curve
rarefactionViable :: [Double] -> Double
rarefactionViable !xs = (genericLength valid / genericLength xs) * 100
  where
    valid = dropWhile (< (0.95 * last xs)) xs

-- | Returns the number of individuals needed to get the proportion g of
-- the estimated total richness of the assemblage. Sobs / Sest < g < 1
individualG :: Double -> Map.Map (Sample, Fragment) Int -> Double
individualG g sample
    | sobs / sest >= g = 0
    | otherwise = ((sobs * f1) / (2 * f2)) * log (f0 / ((1 - g) * sest))
  where
    sest = sobs + f0
    sobs = fromIntegral $ richness sample
    f2   = fromIntegral . abundanceFreq 2 $ sample
    f1   = fromIntegral . abundanceFreq 1 $ sample
    f0   = chao1 sample

-- | Returns the number of samples needed to get the proportion g of
-- the estimated total richness of the assemblage. Sobs / Sest < g < 1
sampleG :: Double -> Map.Map (Sample, Fragment) Int -> Double
sampleG g sample
    | sobs / sest >= g = 0
    | otherwise = log (1 - ((t / (t - 1)) * ((2 * q2) / (q1 ** 2)) * ((g * sest) - sobs)))
                / log (1 - ((2 * q2) / (((t - 1) * q1) + (2 * q2))))
  where
    t    = fromIntegral . Map.size . Map.mapKeys fst $ sample
    sest = sobs + q0
    sobs = fromIntegral $ richness sample
    q2   = fromIntegral . overlapFreq 2 $ sample
    q1   = fromIntegral . overlapFreq 1 $ sample
    q0   = chao2 sample

-- | Returns the number of samples needed before a new sample returns less
-- than x new species. Warning, goes forever until threshold is met!
minRarefaction :: Bool
               -> Bool
               -> Int
               -> Map.Map (Sample, Fragment) Int
               -> Double
               -> Int
               -> IO Int
minRarefaction _ _ (-1) _ _ _ = return (-1)
minRarefaction bySample fastBin threshold sample !oldRare !count = do
    newRare <- fmap (maybe (-1) fst . snd . head)
             . rarefaction bySample count
             $ sample
    if newRare == (-1)
        then return (-1)
        else if newRare - oldRare < fromIntegral threshold
            then return count
            else minRarefaction True fastBin threshold sample newRare (count + 1)
  where
    rarefaction True x  = rarefactionSampleCurve fastBin x 1 x
    rarefaction False x = rarefactionCurve
                          fastBin
                          0
                          (fromIntegral x)
                          1
                          (fromIntegral x)

