module Day14.RegolithReservoir (sandUnits, showReservoir) where

import Day14.Reservoir (empty, insertScans)

-- How many units of sand come to rest before sand starts flowing into the abyss below?
sandUnits :: String -> Int
sandUnits = undefined

showReservoir :: String -> String
showReservoir = show . (`insertScans` empty) . read
