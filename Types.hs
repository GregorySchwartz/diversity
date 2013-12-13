-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types.

module Types where

import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: String
                                   , fastaSeq  :: String
                                   } deriving (Eq, Ord, Show)

-- Basic
type Fragment  = String
type Position  = Int
type Diversity = Double
type Order     = Double
type Label     = String

-- Advanced
type PositionMap     = M.Map Position [Fragment]
type DiversityMap    = M.Map Position Diversity
