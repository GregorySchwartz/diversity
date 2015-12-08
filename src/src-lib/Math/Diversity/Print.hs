-- Print module
-- By G.W. Schwartz
--
{- | Collection of functions for the printing of data (converting data
structures into strings for use with writing to output files).
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.Print ( printDiversity
                            , printRarefaction
                            , printRarefactionCurve ) where

-- Built in
import Data.List
import qualified Data.Map.Strict as Map

-- Local
import Math.Diversity.Types
import Math.Diversity.Diversity

-- Return the results of the diversity analysis in string form for saving
-- to a file
printDiversity :: Label -> Order -> Window -> PositionMap -> String
printDiversity label order window positionMap = header ++ body
  where
    header           = "label,order,window,position,weight,diversity\n"
    body             = unlines
                     . map mapLine
                     . Map.toAscList
                     $ positionMap
    mapLine (p, xs) = intercalate "," . line p $ xs
    line p xs = [ label
                , show order
                , show window
                , show p
                , show . Map.foldl' (+) 0 $ xs
                , show . diversityOfMap order $ xs
                ]

-- Return the results of the rarefaction analysis in string form for saving
-- to a file
printRarefaction :: Bool
                 -> Double
                 -> Label
                 -> Window
                 -> PositionMap
                 -> IO String
printRarefaction
    bySample g label window positionMap = do
    body <- fmap unlines . mapM mapLine . Map.toAscList $ positionMap
    return (header ++ body)
  where
    header           = "label,window,position,weight,\
                       \additional_sampling,g_proportion,richness,\
                       \S_est,S_est_var\n"
    mapLine (p, xs)  = fmap (intercalate ",") . line p $ xs
    line p xs        = do
        -- The minimum number of samples needed before any additional
        -- sampling returns less than the threshold (min) number of species
        return [ label
               , show window
               , show p
               , show . Map.foldl' (+) 0 $ xs
               , show . additionalSampling bySample $ xs
               , show g
               , show . sobs $ xs
               , show . sest bySample $ xs
               , show . var bySample $ xs
               ]
    sest True xs  = sobs xs + chao2 xs
    sest False xs = sobs xs + chao1 xs
    var True      = chao2Var
    var False     = chao1Var
    sobs = fromIntegral . richness
    additionalSampling True  = sampleG g
    additionalSampling False = individualG g

-- Return the results of the rarefaction analysis of the entire curve in
-- string form for saving to a file
printRarefactionCurve :: Bool
                      -> Bool
                      -> Bool
                      -> Int
                      -> Int
                      -> Int
                      -> Int
                      -> Label
                      -> Window
                      -> PositionMap
                      -> IO String
printRarefactionCurve
    asDF bySample fastBin runs start interval end label window positionMap = do
    body <- fmap unlines . mapM mapLine . Map.toAscList $ positionMap

    return (header asDF ++ body)
  where
    header False      = "label,window,position,weight,percent_above,\
                        \expected_richness,mad\n"
    header True       = "label,window,position,weight,percent_above,subsample,\
                        \expected_richness,mad\n"
    mapLine (!p, !xs) = line asDF p xs
    line False p xs   = do
        curve <- getRarefactionCurve bySample xs
        return . intercalate "," $ [ label
                                   , show window
                                   , show p
                                   , show . Map.foldl' (+) 0 $ xs
                                   , show
                                   . rarefactionViable
                                   . map (snd . snd)
                                   $ curve
                                   , intercalate "/"
                                   . map (show . fst . snd)
                                   $ curve
                                   , intercalate "/"
                                   . map (show . snd . snd)
                                   $ curve
                                   ]
    line True p xs    = do
        curve <- getRarefactionCurve bySample xs
        return . intercalate "\n"
               . map ( \(!x, (!y, !z))
                     -> intercalate "," [ label
                                        , show window
                                        , show p
                                        , show . Map.foldl' (+) 0 $ xs
                                        , show
                                        . rarefactionViable
                                        . map (snd . snd)
                                        $ curve
                                        , show x
                                        , show y
                                        , show z
                                        ] )
               $ curve
    getRarefactionCurve True = rarefactionSampleCurve fastBin start interval end
    getRarefactionCurve False = rarefactionCurve
                                fastBin
                                runs
                                (fromIntegral start)
                                (fromIntegral interval)
                                (fromIntegral end)
