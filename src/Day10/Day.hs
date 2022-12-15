module Day10.Day (day10part1, day10part2) where

-- https://adventofcode.com/2022/day/10

import Common.Day
import Common.TextFile (readFileContents)
import Day10.CathodeRayTube
import Text.Printf (printf)

day10part1 :: Day
day10part1 =
  Day
    { name = "day10part1",
      friendlyName = "Day 10 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day10.txt"
        let result = signalStrengthSum input
        print result
    }

day10part2 :: Day
day10part2 =
  Day
    { name = "day10part2",
      friendlyName = "Day 10 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day10.txt"
        let result = crtOutput input
        printf "\n%s\n" result
    }
