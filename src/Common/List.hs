module Common.List (lastN, distinct, isDistinct, safeHead) where

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

isDistinct :: Ord a => [a] -> Bool
isDistinct xs = (length xs ==) . length . distinct $ xs

-- https://stackoverflow.com/a/29266342
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
