module Day02 where

import Control.Applicative

parse :: [String] -> [[Int]]
parse = map (map read . words)

part1 :: [Int] -> Int
part1 xs = maximum xs - minimum xs

part2 :: [Int] -> Int
part2 xs = head $ map fst filtered
  where
    combinations = liftA2 (,) xs xs
    divmods      = map (\t -> fst t `divMod` snd t) combinations
    filtered     = filter (\t -> snd t == 0 && fst t > 1) divmods

main :: IO ()
main = do
  rows <- parse . lines <$> readFile "input/02.txt"
  print $ sum $ map part1 rows
  print $ sum $ map part2 rows
