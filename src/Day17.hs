module Day17 where

import Data.Maybe (fromJust)
import Data.List (elemIndex)

type StepTup = (Int, Int, [Int])

next :: Int -> StepTup -> StepTup
next steps (num, pos, buffer) = (num + 1, newPos, newBuffer)
  where
    newPos    = 1 + (pos + steps) `mod` num
    newBuffer = take (newPos) buffer ++ [num] ++ drop (newPos) buffer

main :: IO ()
main = do
  f <- readFile "input/17.txt"
  let steps              = read f :: Int
      (_, pos1, buffer1) = iterate (next steps) (1, 0, [0]) !! 2017
      (_, _, buffer2)    = iterate (next steps) (1, 0, [0]) !! 50000000
  print $ buffer1 !! (pos1 + 1)
  print $ buffer2 !! (1 + (fromJust $ elemIndex 0 buffer2))
