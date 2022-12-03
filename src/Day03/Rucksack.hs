module Day03.Rucksack (Rucksack (Rucksack), readOne, readMany, itemInBoth, contains, items) where

import Data.List (nub)
import Day03.Item (Item, fromMany)

data Rucksack = Rucksack [Item] [Item] deriving (Show, Eq)

readOne :: String -> Rucksack
readOne s = Rucksack left right
  where
    halfSize = (`div` 2) . length $ s
    (left, right) = splitAt halfSize . fromMany $ s

readMany :: String -> [Rucksack]
readMany = map readOne . lines

itemInBoth :: Rucksack -> Maybe Item
itemInBoth (Rucksack left right)
  | null found = Nothing
  | otherwise = Just . head $ found
  where
    found = filter (`elem` left) . nub $ right

contains :: Rucksack -> Item -> Bool
contains (Rucksack left right) i = i `elem` left || i `elem` right

items :: Rucksack -> [Item]
items (Rucksack left right) = nub left ++ nub right
