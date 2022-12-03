module Day03.RucksackReorganization (prioritySum) where

import Data.Maybe (mapMaybe)
import Day03.Item
import Day03.Rucksack

prioritySum :: String -> Int
prioritySum = sum . map priority . mapMaybe itemInBoth . readMany
