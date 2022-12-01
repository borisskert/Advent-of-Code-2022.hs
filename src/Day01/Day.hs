module Day01.Day (day01part1, day01part2) where

-- https://adventofcode.com/2022/day/1

import Common.Day
import Common.TextFile (readFileContents)
import Day01.CalorieCounting (maximumCalories, topThreeSumCalories)

day01part1 :: Day
day01part1 =
  Day
    { name = "day01part1",
      friendlyName = "Day 01 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day01.txt"
        let result = maximumCalories input
        print result
    }

day01part2 :: Day
day01part2 =
  Day
    { name = "day01part2",
      friendlyName = "Day 01 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day01.txt"
        let result = topThreeSumCalories input
        print result
    }
