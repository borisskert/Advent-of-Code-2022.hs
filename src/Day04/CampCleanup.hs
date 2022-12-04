module Day04.CampCleanup (howManyPairsFullyContainAnother) where

import Day04.AssignmentPair

howManyPairsFullyContainAnother :: String -> Int
howManyPairsFullyContainAnother = length . filter isFullyContained . readMany
