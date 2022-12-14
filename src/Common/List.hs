module Common.List
  ( lastN,
    distinct,
    distinctOn,
    isDistinct,
    safeHead,
    groupOn,
    takeUntil,
    takeAscending,
    takeAscendingBy,
    takeAscendingOn,
    append,
    minimumOn,
    maximumOn,
    minimumOr,
    maximumOr,
    minimumMaybe,
    zipWithIndex,
    range,
    zigzag,
    minimumMaximum,
    minimumMaximumBy,
    minimumMaximumOn,
  )
where

import Data.List (groupBy, maximumBy, minimumBy)
import qualified Data.Map as Map (empty, insert, member)
import qualified Data.Set as Set (empty, insert, member)

--  https://stackoverflow.com/a/17253092
zipLeftover :: [a] -> [a] -> [a]
zipLeftover [] [] = []
zipLeftover xs [] = xs
zipLeftover [] ys = ys
zipLeftover (_ : xs) (_ : ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

-- https://stackoverflow.com/a/3100764
distinct :: Ord a => [a] -> [a]
distinct = create Set.empty
  where
    create s (x : xs')
      | x `Set.member` s = create s xs'
      | otherwise = x : create (Set.insert x s) xs'
    create _ _ = []

distinctOn :: (Ord b) => (a -> b) -> [a] -> [a]
distinctOn fn = create Map.empty
  where
    create s (x : xs')
      | fn x `Map.member` s = create s xs'
      | otherwise = x : create (Map.insert (fn x) x s) xs'
    create _ _ = []

isDistinct :: Ord a => [a] -> Bool
isDistinct xs = (length xs ==) . length . distinct $ xs

-- https://stackoverflow.com/a/29266342
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn get = groupBy (\a b -> get a == get b)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil predicate (x : xs)
  | predicate x = [x]
  | otherwise = x : takeUntil predicate xs

takeAscending :: (Ord a) => [a] -> [a]
takeAscending = go []
  where
    go [] (x : xs) = go [x] xs
    go ys [] = ys
    go ys (x : xs)
      | last ys < x = go (ys ++ [x]) xs
      | otherwise = go ys xs

takeAscendingBy :: (a -> a -> Ordering) -> [a] -> [a]
takeAscendingBy cmpFn = go []
  where
    go [] (x : xs) = go [x] xs
    go ys [] = ys
    go ys (x : xs)
      | cmpFn (last ys) x == LT = go (ys ++ [x]) xs
      | otherwise = go ys xs

takeAscendingOn :: (Ord b) => (a -> b) -> [a] -> [a]
takeAscendingOn fn = go []
  where
    go [] (x : xs) = go [x] xs
    go ys [] = ys
    go ys (x : xs)
      | fn (last ys) < fn x = go (ys ++ [x]) xs
      | otherwise = go ys xs

append :: a -> [a] -> [a]
append x xs = xs ++ [x]

minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn fn = minimumBy (\a b -> compare (fn a) (fn b))

maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn fn = maximumBy (\a b -> compare (fn a) (fn b))

minimumOr :: (Ord a) => a -> [a] -> a
minimumOr x [] = x
minimumOr _ xs = minimum xs

maximumOr :: (Ord a) => a -> [a] -> a
maximumOr x [] = x
maximumOr _ xs = maximum xs

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe xs = Just $ minimum xs

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

range :: Int -> Int -> [Int]
range from to
  | from == to = [from]
  | from < to = [from .. to]
  | otherwise = [from, from - 1 .. to]

-- Creates a zigzag pattern starting from the given point
-- Example: zigzag 10 -> [10,11,9,12,8,13,7,14,6,15]
zigzag :: Integral a => a -> [a]
zigzag n = scanl (\x (sign, i) -> x + i * sign) n . zip signums $ [1 ..]
  where
    signums = cycle [1, -1]

minimumMaximum :: (Ord a) => [a] -> (a, a)
minimumMaximum [] = error "minimumMaximum: empty list"
minimumMaximum [x] = (x, x)
minimumMaximum (x : xs) = (min x minRest, max x maxRest)
  where
    (minRest, maxRest) = minimumMaximum xs

minimumMaximumBy :: (a -> a -> Ordering) -> [a] -> (a, a)
minimumMaximumBy _ [] = error "minimumMaximumBy: empty list"
minimumMaximumBy _ [x] = (x, x)
minimumMaximumBy cmpFn (x : xs) = (minX, maxX)
  where
    (minRest, maxRest) = minimumMaximumBy cmpFn xs
    minX
      | cmpFn x minRest == LT = x
      | otherwise = minRest
    maxX
      | cmpFn x maxRest == GT = x
      | otherwise = maxRest

minimumMaximumOn :: (Ord b) => (a -> b) -> [a] -> (a, a)
minimumMaximumOn fn = minimumMaximumBy (\a b -> compare (fn a) (fn b))
