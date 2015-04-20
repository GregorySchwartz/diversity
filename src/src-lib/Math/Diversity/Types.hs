-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types.
-}

module Math.Diversity.Types where

import qualified Data.Map.Strict as Map

-- Basic
type Fragment  = String
type Sample    = String
type Position  = Int
type Diversity = Double
type Order     = Double
type Label     = String
type Window    = Int

-- Advanced
type FrequencyMap    = Map.Map (Sample, Fragment) Int
-- | At each position we have a collection of fragments to find the
-- diversity of
type PositionMap     = Map.Map Position FrequencyMap
-- | At each position we have a diversity
type DiversityMap    = Map.Map Position Diversity
