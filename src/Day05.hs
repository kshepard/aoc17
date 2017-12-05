module Day05 where

import Prelude hiding (length)
import Data.Sequence

parse :: [String] -> [Int]
parse = map read

maze :: Seq Int -> (Int -> Int) -> Int
maze ints getVal = go 0 0 ints
  where
    go ind steps ints'
      | ind < 0            = steps
      | ind >= length ints = steps
      | otherwise          = go newInd (steps + 1) newInts
      where
        currVal = index ints' ind
        newInd  = ind + currVal
        newVal  = getVal currVal
        newInts = update ind newVal ints'

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/05.txt"
  let ints = fromList input
  print $ maze ints (+1)
  print $ maze ints (\x -> if x >= 3 then x - 1 else x + 1)
