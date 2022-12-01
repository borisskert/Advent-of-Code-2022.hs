module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO
import Day01.Day (day01)

allDays :: [Day]
allDays =
  [ 
    day01
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
