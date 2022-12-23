module Common.Maybe (maybeDo, replace, maybeReplace, maybeNothing, maybeSkip) where

import Data.Maybe (fromMaybe)

maybeDo :: (a -> a -> a) -> Maybe a -> a -> a
maybeDo _ Nothing x = x
maybeDo fn (Just y) x = fn y x

replace :: (a -> Maybe a) -> a -> a
replace fn x = fromMaybe x (fn x)

maybeReplace :: (a -> a -> Maybe a) -> Maybe a -> a -> a
maybeReplace _ Nothing x = x
maybeReplace fn (Just y) x = fromMaybe x (fn y x)

maybeNothing :: (a -> a -> Maybe a) -> Maybe a -> a -> Maybe a
maybeNothing _ Nothing _ = Nothing
maybeNothing fn (Just y) x = fn y x

maybeSkip :: (a -> a -> Maybe a) -> Maybe a -> a -> Maybe a
maybeSkip _ Nothing x = Just x
maybeSkip fn (Just y) x = fn y x
