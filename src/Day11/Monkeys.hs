module Day11.Monkeys (Monkeys, fromList, playRound, playRounds, insert, toList) where

import Common.Fold
import Common.Map
import Common.Prelude
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map (elems, keys)
import qualified Day11.Item as Item (normalize)
import Day11.ItemThrow (ItemThrow, item, to)
import Day11.Monkey (Monkey)
import qualified Day11.Monkey as Monkey (catch, id, inspectItems, test)
import Day11.MonkeyId
import qualified Day11.Test as Test (divisibleBy)
import Day11.WorryLevel (WorryLevel)
import Prelude hiding (lookup)

data Monkeys = Monkeys (Map MonkeyId Monkey) WorryLevel deriving (Eq, Show)

instance Read Monkeys where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseFromInput input

parseFromInput :: String -> Monkeys
parseFromInput input = Monkeys monkeyMap maxWorry
  where
    monkeys = map (read . unlines) . splitOn [""] . lines $ input
    maxWorry = lcmOf . map (Test.divisibleBy . Monkey.test) $ monkeys
    monkeyMap = fromListOn Monkey.id monkeys

fromList :: [Monkey] -> Monkeys
fromList monkeys = Monkeys monkeyMap maxWorry
  where
    maxWorry = lcmOf . map (Test.divisibleBy . Monkey.test) $ monkeys
    monkeyMap = fromListOn Monkey.id monkeys

playRounds :: WorryLevel -> Int -> Monkeys -> Monkeys
playRounds worry n monkeys = times (playRound worry) monkeys n

playRound :: WorryLevel -> Monkeys -> Monkeys
playRound worry monkeys@(Monkeys monkeyMap _) = (`go` monkeys) . Map.keys $ monkeyMap
  where
    go :: [MonkeyId] -> Monkeys -> Monkeys
    go [] monkeys' = monkeys'
    go (myId : others) monkeys' = go others . (`handle` monkeys') . Monkey.inspectItems worry . (`lookup` monkeys') $ myId

handle :: ([ItemThrow], Monkey) -> Monkeys -> Monkeys
handle (itemThrows, monkey) monkeys = catches itemThrows nextMonkeys
  where
    nextMonkeys = insert monkey monkeys

catches :: [ItemThrow] -> Monkeys -> Monkeys
catches ts monkeys = foldl (flip catch) monkeys ts

catch :: ItemThrow -> Monkeys -> Monkeys
catch t monkeys@(Monkeys _ maxWorry) =
  ((`insert` monkeys) . Monkey.catch myItem)
    . lookup myId
    $ monkeys
  where
    myId = to t
    myItem = Item.normalize maxWorry . item $ t

lookup :: MonkeyId -> Monkeys -> Monkey
lookup monkeyId (Monkeys monkeys _) = (!) monkeys monkeyId

insert :: Monkey -> Monkeys -> Monkeys
insert monkey (Monkeys monkeys maxWorry) = (`Monkeys` maxWorry) . insertOn Monkey.id monkey $ monkeys

toList :: Monkeys -> [Monkey]
toList (Monkeys monkeys' _) = Map.elems monkeys'
