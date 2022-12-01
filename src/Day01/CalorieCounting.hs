module Day01.CalorieCounting (maximumCalories, topThreeSumCalories) where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down))

maximumCalories :: String -> Int
maximumCalories = maximum . readCalories

topThreeSumCalories :: String -> Int
topThreeSumCalories = sum . take 3 . sortOn Down . readCalories

readCalories :: String -> [Int]
readCalories = map (sum . map (read :: String -> Int)) . splitOn [""] . lines
