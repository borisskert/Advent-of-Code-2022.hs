module Common.ShortestPathFast (findRoute) where

import qualified Data.Set as Set

-- https://stackoverflow.com/a/64322395/13213024
findRoute
    :: (Ord cost , Ord node)
    => ((cost , node) -> [(cost , node)]) -- ^ Where we can go from a node and the cost of that
    -> (node -> Bool)                     -- ^ Where we want to get to
    -> (cost , node)                      -- ^ The start position
    -> Maybe (cost , node)                -- ^ Maybe the answer. Maybe it doesn't exist
findRoute next isTarget start = search mempty (Set.singleton start)
    where
        search visited toBeVisited = case Set.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost , vertex) , withoutVertex)
                | isTarget vertex             -> Just (cost , vertex)
                | vertex `Set.member` visited -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = Set.insert vertex visited
                    withNext = foldr Set.insert withoutVertex $ next (cost , vertex)
