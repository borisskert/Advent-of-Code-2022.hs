module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO
import Day01.Day (day01part1, day01part2)

allDays :: [Day]
allDays =
  [ day01part1,
    day01part2
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
