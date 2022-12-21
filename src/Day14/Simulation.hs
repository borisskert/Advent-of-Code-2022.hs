module Day14.Simulation (from, oneDrop, reservoir, sandUnits, dropPoint) where

import qualified Common.Grid as Grid (fromTuple, member, x, y)
import Common.OctaGridPosition (Position)
import Day14.Reservoir (Reservoir)
import qualified Day14.Reservoir as Reservoir (findSandSource, depthAt, insertSandAt, sandUnits, toGrid)
import Debug.Trace (traceShow)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simulation data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Simulation = Simulation Reservoir deriving (Show, Eq)

from :: Reservoir -> Simulation
from = Simulation

oneDrop :: Simulation -> Simulation
oneDrop simulation@(Simulation reservoir') = Simulation . Reservoir.insertSandAt (dropPoint simulation) $ reservoir'

reservoir :: Simulation -> Reservoir
reservoir (Simulation myReservoir) = myReservoir

sandUnits :: Simulation -> [Position]
sandUnits = Reservoir.sandUnits . reservoir

dropPoint :: Simulation -> Position
dropPoint (Simulation reservoir')
  | Grid.member designatedDropPosition grid = traceShow(source, x', depth) designatedDropPosition
  | otherwise = traceShow(source, x', depth) designatedDropPosition
  where
    grid = Reservoir.toGrid reservoir'
    source = Reservoir.findSandSource reservoir'
    x' = Grid.x source
    depth = Reservoir.depthAt x' reservoir'
    designatedDropPosition = Grid.fromTuple (x', depth - 1)
