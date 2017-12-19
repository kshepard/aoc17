module Day19 where

import Data.Char (isLetter)
import Data.List (delete, elemIndex)
import Data.Maybe (fromJust)

type Coord = (Int, Int)

data Dir = U | D | L | R deriving (Eq)

nextCoord :: Dir -> Coord -> Coord
nextCoord U (x, y) = (x, y - 1)
nextCoord D (x, y) = (x, y + 1)
nextCoord L (x, y) = (x - 1, y)
nextCoord R (x, y) = (x + 1, y)

oppDir :: Dir -> Dir
oppDir U = D
oppDir D = U
oppDir L = R
oppDir R = L

move :: [String] -> (String, Int)
move m = go start D "" 0
  where
    maxY  = length m
    maxX  = length $ head m
    start = (fromJust $ elemIndex '|' $ head m, 0)
    getVal (x, y)
      | x < 0 || y < 0 || x >= maxX || y >= maxY = ' '
      | otherwise = m !! y !! x
    go c d acc steps
      | currV == ' '   = (acc, steps)
      | currV == '+'   = go newC newD acc (steps + 1)
      | isLetter currV = go nextC d (acc ++ [currV]) (steps + 1)
      | otherwise      = go nextC d acc (steps + 1)
      where
        currV = getVal c
        nextC = nextCoord d c
        dirs  = delete (oppDir d) [U, D, L, R]
        newDs = [ x | x <- dirs, getVal (nextCoord x c) /= ' ' ]
        newD  = if length newDs == 0 then d else newDs !! 0
        newC  = nextCoord newD c

main :: IO ()
main = do
  matrix <- init . lines <$> readFile "input/19.txt"
  let (str, steps) = move matrix
  print str
  print steps
