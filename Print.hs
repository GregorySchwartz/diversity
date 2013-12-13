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

-- Return the results of the diversity analysis in string form for saving
-- to a file
printDiversity :: Label -> Order -> DiversityMap -> String
printDiversity label order diversityMap = header ++ body
  where
    header           = "label,order,position,diversity\n"
    body             = unlines                          .
                       map mapLine                      .
                       M.toAscList                      $
                       diversityMap
    mapLine (p, d) = intercalate "," [label, show order, show p, show d]
