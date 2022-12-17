module Day11.Monkeys (Monkeys, fromList, playRound, playRounds, insert, toList) where

import Common.Fold
import Common.Map
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map (elems, fromList, keys, lookup, toList)
import Day11.ItemThrow (ItemThrow, item, to)
import Day11.Monkey (Monkey)
import qualified Day11.Monkey as Monkey (catch, id, inspectItems)
import Day11.MonkeyId
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

newtype Monkeys = Monkeys (Map MonkeyId Monkey) deriving (Eq, Show)

instance Read Monkeys where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseFromInput input

parseFromInput :: String -> Monkeys
parseFromInput = Monkeys . fromListOn Monkey.id . map (read . unlines) . splitOn [""] . lines

fromList :: [Monkey] -> Monkeys
fromList = Monkeys . fromListOn Monkey.id

playRounds :: Int -> Monkeys -> Monkeys
playRounds n monkeys = times playRound monkeys n

playRound :: Monkeys -> Monkeys
playRound monkeys@(Monkeys monkeyMap) = (`go` monkeys) . Map.keys $ monkeyMap
  where
    go :: [MonkeyId] -> Monkeys -> Monkeys
    go [] monkeys' = monkeys'
    go (myId : others) monkeys' = go others . (`handle` monkeys') . Monkey.inspectItems . (\x -> traceShow ("go", monkeys') x) . (`lookup` monkeys') $ myId

handle :: ([ItemThrow], Monkey) -> Monkeys -> Monkeys
handle (itemThrows, monkey) monkeys = catches itemThrows nextMonkeys
  where
    nextMonkeys = insert monkey monkeys

catches :: [ItemThrow] -> Monkeys -> Monkeys
catches ts monkeys = foldl (flip catch) monkeys ts

catch :: ItemThrow -> Monkeys -> Monkeys
catch t monkeys@(Monkeys monkeyMap) =
  traceShow (t)
    . ((`insert` monkeys) . Monkey.catch myItem)
    . lookup myId
    $ monkeys
  where
    myId = to t
    myItem = item t

lookup :: MonkeyId -> Monkeys -> Monkey
lookup monkeyId (Monkeys monkeys) = (!) monkeys monkeyId

insert :: Monkey -> Monkeys -> Monkeys
insert monkey (Monkeys monkeys) = traceShow (monkey) . Monkeys . insertOn Monkey.id monkey $ monkeys

toList :: Monkeys -> [Monkey]
toList (Monkeys monkeys') = Map.elems monkeys'
