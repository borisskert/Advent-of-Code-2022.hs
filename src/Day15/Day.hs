{-# LANGUAGE NumericUnderscores #-}

module Day15.Day (day15part1, day15part2) where

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
        let result = howManyPositions 2_000_000 input
        print result
    }

day15part2 :: Day
day15part2 =
  Day
    { name = "day15part2",
      friendlyName = "Day 15 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day15.txt"
        let result = tuningFrequency 2_000_000 input
        print result
    }
