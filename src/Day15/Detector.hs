module Day15.Detector (Detector, from, scanFor, noBeacons, scanAll, toMap) where

import Common.List (minimumOn)
import Common.OctaGridPosition (Position, manhattanDistance, withinManhattanDistance)
import Day15.ComprehensiveMap

newtype Detector = Detector ComprehensiveMap deriving (Eq, Show)

from :: ComprehensiveMap -> Detector
from = Detector

toMap :: Detector -> ComprehensiveMap
toMap (Detector myMap) = myMap

scanFor :: Position -> Detector -> Detector
scanFor sensorPos (Detector myMap) = Detector . foldl (flip clearArea) myMap $ area
  where
    allBeacons = beacons myMap
    nearestBeacon = minimumOn (manhattanDistance sensorPos) allBeacons
    distance = manhattanDistance sensorPos nearestBeacon
    area = withinManhattanDistance distance sensorPos

scanAll :: Detector -> Detector
scanAll detector@(Detector myMap) = foldl (flip scanFor) detector allSensors
  where
    allSensors = sensors myMap

noBeacons :: Detector -> [Position]
noBeacons (Detector myMap) = noDevices myMap
