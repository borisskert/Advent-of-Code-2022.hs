module Day03.RucksackReorganization (prioritySum, badgePrioritySum) where

import Data.Maybe (mapMaybe)
import qualified Day03.Group as Group
import Day03.Item
import Day03.Rucksack

prioritySum :: String -> Int
prioritySum = sum . map priority . mapMaybe itemInBoth . readMany

badgePrioritySum :: String -> Int
badgePrioritySum = sum . map (priority . Group.badge) . Group.readMany
