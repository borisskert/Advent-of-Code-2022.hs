module Day11.ItemThrow (ItemThrow, from, item, to) where

import Day11.Item (Item)
import Day11.MonkeyId (MonkeyId)

data ItemThrow = ItemThrow Item MonkeyId deriving (Eq, Show)

from :: Item -> MonkeyId -> ItemThrow
from = ItemThrow

item :: ItemThrow -> Item
item (ItemThrow i _) = i

to :: ItemThrow -> MonkeyId
to (ItemThrow _ monkeyId) = monkeyId
