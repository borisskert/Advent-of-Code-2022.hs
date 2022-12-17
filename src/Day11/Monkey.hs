{-# LANGUAGE TupleSections #-}

module Day11.Monkey (Monkey, from, inspectItem, inspectItems, businessLevel, catch, id) where

import Common.List
import Day11.Item (Item)
import Day11.ItemThrow (ItemThrow)
import qualified Day11.ItemThrow as Throw (from)
import Day11.Items (Items, current, remaining)
import qualified Day11.Items as Items (append)
import Day11.MonkeyId (MonkeyId)
import Day11.Operation (Operation, operate)
import Day11.Test (Test, decide)
import Prelude hiding (id)

type BusinessLevel = Int

data Monkey = Monkey {id :: MonkeyId, items :: Items, operation :: Operation, test :: Test, businessLevel :: BusinessLevel} deriving (Eq, Show)

instance Read Monkey where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseFromLines . lines $ input

from :: MonkeyId -> Items -> Operation -> Test -> Int -> Monkey
from myId myItems myOperation myTest myBusinessLevel = Monkey {id = myId, items = myItems, operation = myOperation, test = myTest, businessLevel = myBusinessLevel}

parseFromLines :: [String] -> Monkey
parseFromLines (headerLine : itemsLine : operationLine : testLines) =
  Monkey {id = read headerLine, items = read itemsLine, operation = read operationLine, test = read . unlines $ testLines, businessLevel = 0}
parseFromLines xs = error ("Day11.Monkey.parseFromLines: Illegal input '" ++ show xs ++ "'")

inspectItem :: Monkey -> Maybe (ItemThrow, Monkey)
inspectItem monkey =
  fmap
    (inspect monkey)
    . current
    . items
    $ monkey

inspect :: Monkey -> Item -> (ItemThrow, Monkey)
inspect monkey@Monkey {operation = myOperation, test = myTest} =
  (,throwOneItem monkey)
    . (\item -> Throw.from item ((`decide` myTest) item))
    . (`operate` myOperation)

throwOneItem :: Monkey -> Monkey
throwOneItem Monkey {id = myId, items = myItems, operation = myOperation, test = myTest, businessLevel = myBusinessLevel} =
  Monkey {id = myId, items = remaining myItems, operation = myOperation, test = myTest, businessLevel = myBusinessLevel + 1}

inspectItems :: Monkey -> ([ItemThrow], Monkey)
inspectItems monkey = create ([], monkey)
  where
    create :: ([ItemThrow], Monkey) -> ([ItemThrow], Monkey)
    create (throws, myMonkey) = maybe (throws, myMonkey) (create . (\(t, m) -> (t `append` throws, m))) . inspectItem $ myMonkey

catch :: Item -> Monkey -> Monkey
catch item Monkey {id = myId, items = myItems, operation = myOperation, test = myTest, businessLevel = myBusinessLevel} =
  Monkey {id = myId, items = Items.append item myItems, operation = myOperation, test = myTest, businessLevel = myBusinessLevel}
