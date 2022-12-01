module Day01.Day (day01) where

-- https://adventofcode.com/2022/day/1

import Common.Day
import Common.TextFile (readFileContents)
import Day01.CalorieCounting (maximumCalories)

day01 :: Day
day01 =
  Day
    { name = "day01",
      friendlyName = "Day 01",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day01part1.txt"
        let result = maximumCalories input
        print result
    }
