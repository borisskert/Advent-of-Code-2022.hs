module Day03.RucksackReorganization (prioritySum, badgePrioritySum) where

import Data.Maybe (mapMaybe)
import Day03.Group (badge)
import qualified Day03.Group as Group (readMany)
import Day03.Item (priority)
import Day03.Rucksack (itemInBoth)
import qualified Day03.Rucksack as Rucksack (readMany)

prioritySum :: String -> Int
prioritySum = sum . map priority . mapMaybe itemInBoth . Rucksack.readMany

badgePrioritySum :: String -> Int
badgePrioritySum = sum . map (priority . badge) . Group.readMany
