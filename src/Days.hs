module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO
import Day01.Day (day01part1, day01part2)
import Day02.Day (day02part1, day02part2)
import Day03.Day (day03part1)

allDays :: [Day]
allDays =
  [ day01part1,
    day01part2,
    day02part1,
    day02part2,
    day03part1
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
