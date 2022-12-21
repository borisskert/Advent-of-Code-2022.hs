module Day14.RegolithReservoir (sandUnitsUntilFall, showReservoir, sandUnitsUntilRest) where

import Day14.Reservoir (empty, insertScans)
import Day14.Simulation (dropAll, from)
import qualified Day14.Simulation as Simulation (sandUnits)

-- How many units of sand come to rest before sand starts flowing into the abyss below?
sandUnitsUntilFall :: String -> Int
sandUnitsUntilFall = length . Simulation.sandUnits . dropAll . from . (`insertScans` empty) . read

-- Using your scan, simulate the falling sand until the source of the sand becomes blocked. How many units of sand come to rest?
sandUnitsUntilRest :: String -> Int
sandUnitsUntilRest = undefined

showReservoir :: String -> String
showReservoir = show .  dropAll . from . (`insertScans` empty) . read
