-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types.
-}

module Math.Diversity.Types where

import qualified Data.Map.Strict as Map
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
type PositionMap     = Map.Map Position (Map.Map (Sample, Fragment) Int)
-- | At each position we have a diversity
type DiversityMap    = Map.Map Position Diversity
