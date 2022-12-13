module Day09.Simulation (Simulation, create, perform, visited, moveTail) where

import Common.OctaGridPosition (Position, fromTuple)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, toList)
import Day09.Motion (Motion)
import qualified Day09.Motion as Motion
import Day09.Rope

data Simulation = Simulation (Set Position) Rope

create :: Int -> Simulation
create ropeLength = Simulation logbook rope
  where
    start = fromTuple (0, 0)
    logbook = Set.insert start Set.empty
    rope = fromList . map (const start) $ [0 .. ropeLength]

perform :: Motion -> Simulation -> Simulation
perform motion sim@(Simulation _ rope) = foldl (flip move) sim motions
  where
    ropeHead = headOf rope
    motions = Motion.go motion ropeHead

move :: Position -> Simulation -> Simulation
move motion (Simulation logbook rope)
  | tailKnot == nextTailKnot = Simulation logbook nextRope
  | otherwise = Simulation newLogbook nextRope
  where
    tailKnot = tag rope
    nextRope = moveTail motion rope
    nextTailKnot = tag nextRope
    newLogbook = Set.insert nextTailKnot logbook

visited :: Simulation -> [Position]
visited (Simulation logbook _) = Set.toList logbook
