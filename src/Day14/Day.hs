module Day14.Day (day14part1) where

-- https://adventofcode.com/2022/day/14

import Common.Day
import Common.TextFile (readFileContents)
import Day14.RegolithReservoir

day14part1 :: Day
day14part1 =
  Day
    { name = "day14part1",
      friendlyName = "Day 14 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day14.txt"
        let result = showReservoir input
        print result
    }
