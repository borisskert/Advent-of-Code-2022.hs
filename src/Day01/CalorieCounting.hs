module Day01.CalorieCounting (maximumCalories) where

import Data.List.Split (splitOn)

maximumCalories :: String -> Int
maximumCalories = maximum . map (sum . map (read :: String -> Int)) . splitOn [""] . lines
