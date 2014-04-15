-- Print module
-- By G.W. Schwartz
--
-- Collection of functions for the printing of data (converting data
-- structures into strings for use with writing to output files).

module Print where

-- Built in
import Data.List
import qualified Data.Map as M

-- Local
import Types
import Diversity

-- Return the results of the diversity analysis in string form for saving
-- to a file
printDiversity :: Label -> Order -> Window -> PositionMap -> String
printDiversity label order window positionMap = header ++ body
  where
    header           = "label,order,window,position,weight,diversity\n"
    body             = unlines                          .
                       map mapLine                      .
                       M.toAscList                      $
                       positionMap
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
printRarefaction :: Label -> Window -> PositionMap -> String
printRarefaction label window positionMap = header ++ body
  where
    header           = "label,window,position,weight,percent_above\n"
    body             = unlines                          .
                       map mapLine                      .
                       M.toAscList                      $
                       positionMap
    mapLine (p, xs) = intercalate "," . line p $ xs
    line p xs  = [ label
                 , show window
                 , show p
                 , show . length $ xs
                 , show . rarefactionViable . rarefactionCurve $ xs
                 ]

-- Return the results of the rarefaction analysis of the entire curve in
-- string form for saving to a file
printRarefactionCurve :: Label -> Window -> PositionMap -> String
printRarefactionCurve label window positionMap = header ++ body
  where
    header           = "label,window,position,weight,curve\n"
    body             = unlines                          .
                       map mapLine                      .
                       M.toAscList                      $
                       positionMap
    mapLine (p, xs) = intercalate "," . line p $ xs
    line p xs  = [ label
                 , show window
                 , show p
                 , show . length $ xs
                 , intercalate "," . map show . rarefactionCurve $ xs
                 ]
