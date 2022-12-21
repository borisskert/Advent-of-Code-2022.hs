module Day14.Simulation (from, oneDrop, reservoir, sandUnits) where

import Common.OctaGridPosition (Position)
import Day14.Reservoir (Reservoir)
import qualified Day14.Reservoir as Reservoir (sandUnits)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simulation data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Simulation = Simulation Reservoir deriving (Show, Eq)

from :: Reservoir -> Simulation
from = Simulation

oneDrop :: Simulation -> Simulation
oneDrop = undefined

reservoir :: Simulation -> Reservoir
reservoir (Simulation myReservoir) = myReservoir

sandUnits :: Simulation -> [Position]
sandUnits = Reservoir.sandUnits . reservoir
