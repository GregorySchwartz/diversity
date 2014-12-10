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
                 -> Label
                 -> Window
                 -> PositionMap
                 -> String
printRarefaction sample fastBin start interval label window positionMap =
    header ++ body
  where
    header           = "label,window,position,weight,percent_above\n"
    body             = unlines
                     . map mapLine
                     . M.toAscList
                     $ positionMap
    mapLine (p, xs) = intercalate "," . line p $ xs
    line p xs  = [ label
                 , show window
                 , show p
                 , show . length $ xs
                 , show
                 . rarefactionViable . map snd . getRarefactionCurve sample $ xs
                 ]
    getRarefactionCurve True = rarefactionSampleCurve fastBin start interval
    getRarefactionCurve False = rarefactionCurve fastBin (fromIntegral start) (fromIntegral interval)

-- Return the results of the rarefaction analysis of the entire curve in
-- string form for saving to a file
printRarefactionCurve :: Bool
                      -> Bool
                      -> Bool
                      -> Int
                      -> Int
                      -> Label
                      -> Window
                      -> PositionMap
                      -> String
printRarefactionCurve asDF sample fastBin start interval label window positionMap =
    header asDF ++ body
  where
    header False     = "label,window,position,weight,curve\n"
    header True      = "label,window,position,weight,subsample,vertical_curve\n"
    body             = unlines
                     . map mapLine
                     . M.toAscList
                     $ positionMap
    mapLine (!p, !xs) = line asDF p xs
    line False p xs   = intercalate "," [ label
                                        , show window
                                        , show p
                                        , show . length $ xs
                                        , intercalate "/" . map (show . snd) . getRarefactionCurve sample $ xs
                                        ]
    line True p xs    = intercalate "\n"
                      . map ( \(!x, !y) -> intercalate "," [ label
                                                           , show window
                                                           , show p
                                                           , show . length $ xs
                                                           , show x
                                                           , show y
                                                           ] )
                      . getRarefactionCurve sample $ xs
    getRarefactionCurve True = rarefactionSampleCurve fastBin start interval
    getRarefactionCurve False =
        rarefactionCurve fastBin (fromIntegral start) (fromIntegral interval)
