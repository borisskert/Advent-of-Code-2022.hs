module Day14.Simulation (from, oneDrop, reservoir, sandUnits, dropPoint) where

import qualified Common.Grid as Grid (eastOf, fromTuple, southEastOf, southOf, southWestOf, westOf, x, y)
import Common.OctaGridPosition (Position)
import Data.Maybe (isJust, isNothing)
import Day14.Reservoir (Reservoir)
import qualified Day14.Reservoir as Reservoir (findSandSource, insertSandAt, sandUnits, toGrid)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simulation data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Simulation = Simulation Reservoir deriving (Eq)

from :: Reservoir -> Simulation
from = Simulation

oneDrop :: Simulation -> Simulation
oneDrop simulation@(Simulation reservoir') = Simulation . Reservoir.insertSandAt (dropPoint simulation) $ reservoir'

reservoir :: Simulation -> Reservoir
reservoir (Simulation myReservoir) = myReservoir

sandUnits :: Simulation -> [Position]
sandUnits = Reservoir.sandUnits . reservoir

dropPoint :: Simulation -> Position
dropPoint simulation@(Simulation reservoir') = dropPointFrom designatedDropPosition simulation
  where
    source = Reservoir.findSandSource reservoir'
    x' = Grid.x source
    y' = Grid.y source
    designatedDropPosition = Grid.fromTuple (x', y' + 1)

dropPointFrom :: Position -> Simulation -> Position
dropPointFrom position simulation@(Simulation reservoir')
  | isNothing s = dropPointFrom sPos simulation
  | isJust sw && isJust se = position
  | isNothing sw = dropPointFrom swPos simulation
  | isNothing se = dropPointFrom sePos simulation
  | isNothing w = dropPointFrom wPos simulation
  | isNothing e = dropPointFrom ePos simulation
  | otherwise = position
  where
    grid = Reservoir.toGrid reservoir'
    (sPos, s) = Grid.southOf position grid
    (swPos, sw) = Grid.southWestOf position grid
    (sePos, se) = Grid.southEastOf position grid
    (wPos, w) = Grid.westOf position grid
    (ePos, e) = Grid.eastOf position grid

-- | -------------------------------------------------------------------------------------------------------------------
-- | instance Show
-- | -------------------------------------------------------------------------------------------------------------------
instance Show Simulation where
  show = show . reservoir
