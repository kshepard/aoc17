module Day03 where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type Input   = Int
type Index   = Int
type Val     = Int
type PartRet = Int
type Coord   = (Int, Int)
type Result  = (Val, Coord)
type Stop    = Val -> Bool
type Lookup  = Map.Map Coord Val
type GetVal  = Lookup -> Index -> Coord -> Val

data Dir = UP | DOWN | LEFT | RIGHT

-- Finds the rotated direction given the current direction
rotate :: Dir -> Dir
rotate UP    = LEFT
rotate DOWN  = RIGHT
rotate LEFT  = DOWN
rotate RIGHT = UP

-- Determines the next coordinate after applying a direction
nextCoord :: Dir -> Coord -> Coord
nextCoord UP    (x, y) = (x, y + 1)
nextCoord DOWN  (x, y) = (x, y - 1)
nextCoord LEFT  (x, y) = (x - 1, y)
nextCoord RIGHT (x, y) = (x + 1, y)

-- Calculates the next direction.
-- This is determined by seeing if we're able to rotate, i.e.
-- is there an empty space in the rotated position? If not,
-- just continue on with the current direction.
nextDir :: Dir -> Coord -> Lookup -> Dir
nextDir dir coord lookup
  | canRotate = rotated
  | otherwise = dir
  where
    rotated   = rotate dir
    next      = nextCoord rotated coord
    canRotate = Map.lookup next lookup == Nothing

-- Creates the spiral.
-- Lookup is seeded with the origin and given a down
-- direction, so we immediately rotate and go right.
spiral :: Input -> Stop -> GetVal -> Result
spiral _ stopCond getVal = go 1 (0, 0) originLookup DOWN
  where
    originLookup = Map.fromList [((0,0), 1)]
    go i coord lookup dir
      | stopCond val = (val, newCoord)
      | otherwise    = go (i + 1) newCoord newLookup newDir
      where
        newDir    = nextDir dir coord lookup
        newCoord  = nextCoord newDir coord
        val       = getVal lookup i newCoord
        newLookup = Map.insert newCoord val lookup

-- Sums values of all adjacents
sumAdjacents :: GetVal
sumAdjacents lookup _ (x, y) = sum $ mapMaybe (`Map.lookup` lookup) adjacents
  where
    adjacents = [(x + dx, y + dy) | dx <- [-1..1] , dy <- [-1..1]]

-- Spirals out until value calculated is equal to the input.
-- Values are just set to the index (+1 since spiral starts at 1).
-- Return the manhattan distance of the coordinate.
part1 :: Input -> PartRet
part1 input = abs x + abs y
  where
    (_, (x, y)) = spiral input (==input) (\_ i _ -> i + 1)

-- Spirals out until value calculated is greater than the input.
-- Values are set to the sum of adjacents.
-- Return the value.
part2 :: Input -> PartRet
part2 input = val
  where
    (val, _) = spiral input (>input) sumAdjacents

main :: IO ()
main = do
  f <- readFile "input/03.txt"
  let input = read f :: Input
  print $ part1 input
  print $ part2 input
