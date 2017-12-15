{-# OPTIONS_GHC -fobject-code -O #-}
module Day15 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split (splitOn)
import Text.Printf (printf)

get16Bits :: Int -> String
get16Bits i = drop (length bin - 16) bin
  where
    bin = printf "%08b" i

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/15.txt")
  let rawStrs   = map (\x -> splitOn " " $ Text.unpack x) $ ls
      genAStart = read $ rawStrs !! 0 !! 4 :: Int
      genBStart = read $ rawStrs !! 1 !! 4 :: Int
      divBy     = 2147483647 :: Int
      genANext  = \x -> mod (x * 16807) divBy
      genBNext  = \x -> mod (x * 48271) divBy
      genAPt1   = map get16Bits $ iterate genANext genAStart
      genBPt1   = map get16Bits $ iterate genBNext genBStart
      pairsPt1  = take 40000000 $ zip genAPt1 genBPt1
      part1     = length $ filter (\(x, y) -> x == y) pairsPt1
      genAPt2   = map get16Bits $ filter (\x -> x `mod` 4 == 0) $ iterate genANext genAStart
      genBPt2   = map get16Bits $ filter (\x -> x `mod` 8 == 0) $ iterate genBNext genBStart
      pairsPt2  = take 5000000 $ zip genAPt2 genBPt2
      part2     = length $ filter (\(x, y) -> x == y) pairsPt2
  print part1
  print part2
