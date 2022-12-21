module Day14.Simulation (from) where

import Day14.Reservoir

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simulation data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Simulation = Simulation Reservoir deriving (Show, Eq)

from :: Reservoir -> Simulation
from = Simulation
