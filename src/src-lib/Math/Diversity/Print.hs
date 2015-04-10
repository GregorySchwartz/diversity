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
import qualified Data.Map as M

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
                     . M.toAscList
                     $ positionMap
    mapLine (p, xs) = intercalate "," . line p $ xs
    line p xs = [ label
                , show order
                , show window
                , show p
                , show . length $ xs
                , show . diversity order $ xs
                ]

-- Return the results of the rarefaction analysis in string form for saving
-- to a file
printRarefaction :: Bool
                 -> Bool
                 -> Int
                 -> Int
                 -> Int
                 -> Label
                 -> Window
                 -> PositionMap
                 -> IO String
printRarefaction
    sample fastBin runs start interval label window positionMap = do
    body <- fmap unlines . mapM mapLine . M.toAscList $ positionMap
    return (header ++ body)
  where
    header           = "label,window,position,weight,percent_above\n"
    mapLine (p, xs)  = fmap (intercalate ",") . line p $ xs
    line p xs        = do
        curve <- getRarefactionCurve sample xs
        return [ label
               , show window
               , show p
               , show . length $ xs
               , show . rarefactionViable . map (snd . snd) $ curve
               ]
    getRarefactionCurve True = rarefactionSampleCurve fastBin start interval
    getRarefactionCurve False = rarefactionCurve
        fastBin runs (fromIntegral start) (fromIntegral interval)

-- Return the results of the rarefaction analysis of the entire curve in
-- string form for saving to a file
printRarefactionCurve :: Bool
                      -> Bool
                      -> Bool
                      -> Int
                      -> Int
                      -> Int
                      -> Label
                      -> Window
                      -> PositionMap
                      -> IO String
printRarefactionCurve
    asDF sample fastBin runs start interval label window positionMap = do
    body <- fmap unlines . mapM mapLine . M.toAscList $ positionMap

    return (header asDF ++ body)
  where
    header False      = "label,window,position,weight,expected_richness,\
                        \mad\n"
    header True       = "label,window,position,weight,subsample,\
                        \expected_richness,mad\n"
    mapLine (!p, !xs) = line asDF p xs
    line False p xs   = do
        curve <- getRarefactionCurve sample xs
        return . intercalate "," $ [ label
                                   , show window
                                   , show p
                                   , show . length $ xs
                                   , intercalate "/"
                                   . map (show . fst . snd)
                                   $ curve
                                   , intercalate "/"
                                   . map (show . snd . snd)
                                   $ curve
                                   ]
    line True p xs    = do
        curve <- getRarefactionCurve sample xs
        return . intercalate "\n"
               . map ( \(!x, (!y, !z))
                     -> intercalate "," [ label
                                        , show window
                                        , show p
                                        , show
                                        . length
                                        $ xs
                                        , show x
                                        , show y
                                        , show z
                                        ] )
               $ curve
    getRarefactionCurve True = rarefactionSampleCurve fastBin start interval
    getRarefactionCurve False = rarefactionCurve
        fastBin runs (fromIntegral start) (fromIntegral interval)
