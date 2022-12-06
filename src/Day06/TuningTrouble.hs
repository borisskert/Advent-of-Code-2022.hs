module Day06.TuningTrouble (findFirstMarker, findCorrectMarker) where

import Day06.Marker

findFirstMarker :: String -> Int
findFirstMarker = maybe 0 size . findIn 4

findCorrectMarker :: String -> Int
findCorrectMarker = maybe 0 size . findIn 14
