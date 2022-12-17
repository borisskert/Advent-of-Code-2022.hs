module Day11.Monkey (Monkey, from) where

import Day11.Items (Items)
import Day11.MonkeyId (MonkeyId)
import Day11.Operation (Operation)
import Day11.Test (Test)
import Prelude hiding (id)

data Monkey = Monkey {id :: MonkeyId, items :: Items, operation :: Operation, test :: Test} deriving (Eq, Show)

instance Read Monkey where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseFromLines . lines $ input

from :: MonkeyId -> Items -> Operation -> Test -> Monkey
from myId myItems myOperation myTest = Monkey {id = myId, items = myItems, operation = myOperation, test = myTest}

parseFromLines :: [String] -> Monkey
parseFromLines (headerLine : itemsLine : operationLine : testLines) =
  Monkey {id = read headerLine, items = read itemsLine, operation = read operationLine, test = read . unlines $ testLines}
parseFromLines xs = error ("Day11.Monkey.parseFromLines: Illegal input '" ++ show xs ++ "'")
