module Day14.RegolithReservoir (sandUnits, showReservoir) where

import Day14.Reservoir (empty, insertScans)
import Day14.Simulation (dropAll, from)
import qualified Day14.Simulation as Simulation (sandUnits)

-- How many units of sand come to rest before sand starts flowing into the abyss below?
sandUnits :: String -> Int
sandUnits = length . Simulation.sandUnits . dropAll . from . (`insertScans` empty) . read

showReservoir :: String -> String
showReservoir = show .  dropAll . from . (`insertScans` empty) . read
