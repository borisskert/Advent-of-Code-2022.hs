module Common.Map (fromElems) where

import Data.Map (Map, fromList)

fromElems :: (Ord k) => (a -> k) -> [a] -> Map k a
fromElems key = fromList . map (\x -> (key x, x))
