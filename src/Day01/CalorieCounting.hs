module Day01.CalorieCounting (maximumCalories, topThreeSumCalories) where

import Data.List.Split (splitOn)
import Data.List (sortOn)
import Data.Ord (Down (Down))

maximumCalories :: String -> Int
maximumCalories = maximum . map (sum . map (read :: String -> Int)) . splitOn [""] . lines

topThreeSumCalories :: String -> Int
topThreeSumCalories = sum . take 3 . sortOn (Down) . map (sum . map (read :: String -> Int)) . splitOn [""] . lines
