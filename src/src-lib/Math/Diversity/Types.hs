-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types.
-}

module Math.Diversity.Types where

import qualified Data.Map as M
import qualified Data.Sequence as Seq

-- Basic
type Fragment  = Seq.Seq Char
type Sample    = String
type Position  = Int
type Diversity = Double
type Order     = Double
type Label     = String
type Window    = Int

-- Advanced
-- | At each position we have a collection of fragments to find the
-- diversity of
type PositionMap     = M.Map Position [(Sample, Fragment)]
-- | At each position we have a diversity
type DiversityMap    = M.Map Position Diversity
