module Day04.CampCleanup (howManyPairsFullyContainAnother, howManyPairsOverlap) where

import Day04.AssignmentPair

howManyPairsFullyContainAnother :: String -> Int
howManyPairsFullyContainAnother = length . filter isFullyContained . readMany

howManyPairsOverlap :: String -> Int
howManyPairsOverlap = length . filter isOverlapped . readMany
