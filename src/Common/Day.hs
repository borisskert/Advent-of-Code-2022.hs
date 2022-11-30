module Common.Day where

import System.IO

data Day = Day {run :: IO (), isDefault :: Bool, name :: String, friendlyName :: String}
