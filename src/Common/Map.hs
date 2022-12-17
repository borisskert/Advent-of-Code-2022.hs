module Common.Map (fromListOn, insertOn) where

import Data.Map (Map, fromList, insert)

fromListOn :: (Ord k) => (a -> k) -> [a] -> Map k a
fromListOn keyFn = fromList . map (\i -> (keyFn i, i))

insertOn :: (Ord k) => (a -> k) -> a -> Map k a -> Map k a
insertOn keyFn item = insert (keyFn item) item
