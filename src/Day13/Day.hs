module Day13.Day (day13part1, day13part2) where

-- https://adventofcode.com/2022/day/13

import Common.Day
import Common.TextFile (readFileContents)
import Day13.DistressSignal

day13part1 :: Day
day13part1 =
  Day
    { name = "day13part1",
      friendlyName = "Day 13 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day13.txt"
        let result = indicesSum input
        print result
    }

day13part2 :: Day
day13part2 =
  Day
    { name = "day13part2",
      friendlyName = "Day 13 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day13.txt"
        let result = decoderKey input
        print result
    }
