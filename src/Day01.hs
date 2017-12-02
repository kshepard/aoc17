module Day01 where

import Data.Char

-- Creates pairs from a list of ints, including
-- a pair for the last and first
mkPairs :: [Int] -> [(Int, Int)]
mkPairs xs = zip full $ tail full
  where
    full = xs ++ [head xs]

-- Creates pairs, but going halfway around the list
mkHalfPairs :: [Int] -> [(Int, Int)]
mkHalfPairs xs = zip xs rotated
  where
    half = div (length xs) 2
    rotated = drop half xs ++ take half xs

-- Filters pairs where the items are equal
filterPairs :: [(Int, Int)] -> [(Int, Int)]
filterPairs ps = filter (\x -> fst x == snd x) ps

-- Sums up pairs (first element of each)
sumPairs :: [(Int, Int)] -> Int
sumPairs ps = sum $ map fst ps

main :: IO ()
main = do
  f <- readFile "input/01.txt"
  let digits = map digitToInt $ init f
  print $ sumPairs . filterPairs . mkPairs $ digits
  print $ sumPairs . filterPairs . mkHalfPairs $ digits
