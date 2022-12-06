module Common.List (lastN, nub, isNub, safeHead) where

import qualified Data.Set as Set

--  https://stackoverflow.com/a/17253092
zipLeftover :: [a] -> [a] -> [a]
zipLeftover [] [] = []
zipLeftover xs [] = xs
zipLeftover [] ys = ys
zipLeftover (_ : xs) (_ : ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

nub :: Ord a => [a] -> [a]
nub = create Set.empty
  where
    create s (x : xs')
      | x `Set.member` s = create s xs'
      | otherwise = x : create (Set.insert x s) xs'
    create _ _ = []

isNub :: Ord a => [a] -> Bool
isNub xs = (length xs ==) . length . nub $ xs

-- https://stackoverflow.com/a/29266342
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
