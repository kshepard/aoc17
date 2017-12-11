module Day11 where

import Data.List.Split (splitOn)

calcDirs :: [String] -> (Int, Int)
calcDirs dirs = go dirs 0 0 0 0
  where
    go dirs' x y z f
      | dirs' == [] = (dist, f)
      | dir == "n"  = go rest x (y + 1) (z - 1) f'
      | dir == "s"  = go rest x (y - 1) (z + 1) f'
      | dir == "ne" = go rest (x + 1) y (z - 1) f'
      | dir == "sw" = go rest (x - 1) y (z + 1) f'
      | dir == "nw" = go rest (x - 1) (y + 1) z f'
      | dir == "se" = go rest (x + 1) (y - 1) z f'
      | otherwise   = error dir
      where
        dir  = head dirs'
        rest = tail dirs'
        dist = (abs x + abs y + abs z) `div` 2
        f'   = max dist f

main :: IO ()
main = do
  input <- readFile "input/11.txt"
  let dirs               = splitOn "," $ init input
      (final, furthest)  = calcDirs dirs
  print final
  print furthest
