module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO

allDays :: [Day]
allDays =
  [ 
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
