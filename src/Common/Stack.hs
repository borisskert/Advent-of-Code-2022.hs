module Common.Stack (Stack, empty, push, top, pop, isEmpty, fromList, pushN, topN, popN) where

newtype Stack a = Stack [a] deriving (Eq, Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

top :: Stack a -> a
top (Stack xs) = head xs

pop :: Stack a -> Stack a
pop (Stack xs) = Stack (tail xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

fromList :: [a] -> Stack a
fromList = Stack

pushN :: [a] -> Stack a -> Stack a
pushN as (Stack xs) = Stack (as ++ xs)

topN :: Int -> Stack a -> [a]
topN n (Stack xs) = take n xs

popN :: Int -> Stack a -> Stack a
popN n (Stack xs) = Stack (drop n xs)
