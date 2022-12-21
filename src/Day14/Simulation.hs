module Day14.Simulation (from, oneDrop, reservoir, sandUnits, dropPoint, dropAll) where

import qualified Common.Grid as Grid (allSouthOf, eastOf, southEastOf, southOf, southWestOf, westOf)
import Common.OctaGridPosition (Position)
import Data.Maybe (isJust, isNothing, fromMaybe, fromJust)
import Day14.Reservoir (Reservoir)
import qualified Day14.Reservoir as Reservoir (findSandSource, insertSandAt, sandUnits, toGrid, isFull)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simulation data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Simulation = Simulation Reservoir deriving (Eq)

from :: Reservoir -> Simulation
from = Simulation

oneDrop :: Simulation -> Simulation
oneDrop simulation = fromMaybe simulation . oneDropMaybe $ simulation

oneDropMaybe :: Simulation -> Maybe Simulation
oneDropMaybe simulation@(Simulation reservoir') = maybe Nothing (Just . Simulation . (`Reservoir.insertSandAt` reservoir')) . dropPoint $ simulation

dropAll :: Simulation -> Simulation
dropAll simulation
  | isNothing dropped = simulation
  | otherwise = dropAll . fromJust $ dropped
  where
    dropped = oneDropMaybe simulation

reservoir :: Simulation -> Reservoir
reservoir (Simulation myReservoir) = myReservoir

sandUnits :: Simulation -> [Position]
sandUnits = Reservoir.sandUnits . reservoir

dropPoint :: Simulation -> Maybe Position
dropPoint simulation@(Simulation reservoir')
   | isNothing designatedDropPosition = Nothing
   | Reservoir.isFull reservoir' = Nothing
   | otherwise = designatedDropPosition
  where
    source = Reservoir.findSandSource reservoir'
    designatedDropPosition = dropPointFrom source simulation

dropPointFrom :: Position -> Simulation -> Maybe Position
dropPointFrom position simulation@(Simulation reservoir')
  | null allSouthOf = Nothing
  | isNothing s = dropPointFrom sPos simulation
  | isJust sw && isJust se = Just position
  | isNothing sw = dropPointFrom swPos simulation
  | isNothing se = dropPointFrom sePos simulation
  | isNothing w = dropPointFrom wPos simulation
  | isNothing e = dropPointFrom ePos simulation
  | otherwise = Just position
  where
    grid = Reservoir.toGrid reservoir'
    (sPos, s) = Grid.southOf position grid
    (swPos, sw) = Grid.southWestOf position grid
    (sePos, se) = Grid.southEastOf position grid
    (wPos, w) = Grid.westOf position grid
    (ePos, e) = Grid.eastOf position grid
    allSouthOf = Grid.allSouthOf position grid

-- | -------------------------------------------------------------------------------------------------------------------
-- | instance Show
-- | -------------------------------------------------------------------------------------------------------------------
instance Show Simulation where
  show = show . reservoir
