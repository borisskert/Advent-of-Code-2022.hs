module Day15.Day (day15part1) where

-- https://adventofcode.com/2022/day/15

import Common.Day
import Common.TextFile (readFileContents)
import Day15.BeaconExclusionZone

day15part1 :: Day
day15part1 =
  Day
    { name = "day15part1",
      friendlyName = "Day 15 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day15.txt"
        let result = howManyPositions 2000000 input
        print result
    }
