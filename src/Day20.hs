module Day20 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (foldl', elemIndex, groupBy, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

next :: [Int] -> [Int]
next p = [px, py, pz, vx, vy, vz, ax, ay, az]
  where
    ax = p !! 6
    ay = p !! 7
    az = p !! 8
    vx = ax + p !! 3
    vy = ay + p !! 4
    vz = az + p !! 5
    px = p !! 0 + p !! 3
    py = p !! 1 + p !! 4
    pz = p !! 2 + p !! 5

nextCollisions :: [[Int]] -> [[Int]]
nextCollisions ps = nextPs
  where
    sorted   = sortOn (take 3) ps
    grouped  = groupBy (\x y -> take 3 x == take 3 y) sorted
    filtered = concat $ filter (\x -> length x == 1) grouped
    nextPs   = map next filtered

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/20.txt")
  let rawStrs   = map (\x -> splitOn " " $ filter (not . (`elem` "pva=<> ")) $ Text.unpack x) $ ls
      splitStrs = map (\x -> splitOn "," x) $ concat rawStrs
      particles = [ map (\x -> read x :: Int ) y | y <- splitStrs ]
      after300  = foldl' (\acc _ -> map next acc) particles ([0..300] :: [Int])
      dists300  = map (\p -> abs (p !! 0) + abs (p !! 1) + abs (p !! 2)) after300
      remaining = foldl' (\acc _ -> nextCollisions acc) particles ([0..300] :: [Int])
  print $ fromJust $ elemIndex (minimum dists300) dists300
  print $ length remaining
