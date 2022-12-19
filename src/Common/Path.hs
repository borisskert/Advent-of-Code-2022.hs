module Common.Path (Path, singleton, fromList, append, steps, toList) where

newtype Path p a = Path [(p, a)]

instance Eq (Path p a) where
  a == b = steps a == steps b

instance Ord (Path p a) where
  compare a b = compare (steps a) (steps b)

instance (Show p, Show a) => Show (Path p a) where
  show (Path xs) = show xs

singleton :: (p, a) -> Path p a
singleton x = Path [x]

steps :: Path p a -> Int
steps (Path xs) = length xs

append :: (p, a) -> Path p a -> Path p a
append pos (Path xs) = Path (xs ++ [pos])

fromList :: [(p, a)] -> Path p a
fromList = Path

toList :: Path p a -> [(p, a)]
toList (Path path) = path
