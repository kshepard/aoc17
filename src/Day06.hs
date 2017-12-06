module Day06 where

import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Sequence
import Prelude hiding (length)

parse :: [String] -> [Int]
parse = head . map (map read . words)

maxIndex :: Seq Int -> Int
maxIndex ints = fromJust maxMaybe
  where
    maxNum   = maximum ints
    maxMaybe = elemIndexL maxNum ints

nextCycle :: Seq Int -> Seq Int
nextCycle ints = go (startInd + 1) (index ints startInd) (update startInd 0 ints)
  where
    startInd = maxIndex ints
    lenInts  = length ints
    go ind count ints'
      | count == 0 = ints'
      | otherwise  = go (realInd + 1) (count - 1) nextInts
      where
        realInd  = if ind >= lenInts then 0 else ind
        realVal  = index ints' realInd
        nextInts = update realInd (realVal + 1) ints'

findCycles :: Seq Int -> (Int, Seq Int)
findCycles input = go input (Set.fromList [input]) 1
  where
    go i s c
     | Set.member next s = (c, next)
     | otherwise         = go next (Set.insert next s) (c + 1)
     where
       next = nextCycle i

main :: IO ()
main = do
  input <- parse . lines <$> readFile "input/06.txt"
  let ints = fromList input
  let (part1Count, s) = findCycles ints
  print part1Count
  print $ fst $ findCycles s
