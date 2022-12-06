module Day06.TuningTrouble (findFirstMarker) where

import Day06.Marker

findFirstMarker :: String -> Int
findFirstMarker = maybe 0 size . findIn
