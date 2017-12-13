module Day13 where

import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

severity :: Int -> Map.Map Int (Int, [Int]) -> Int -> (Int, Bool)
severity maxDepth cfgMap delay = go 0 0 False
  where
    go depth acc wasCaught
      | depth > maxDepth = (acc, wasCaught)
      | otherwise        = go (depth + 1) (acc + (maximum [0, severity'])) (wasCaught || spotted)
      where
        maybeLst = Map.lookup depth cfgMap
        spotted  = severity' > -1
        severity'
          | maybeLst == Nothing = -1
          | caught              = depth * range
          | otherwise           = -1
          where
            (range, lst) = fromJust maybeLst
            caught       = (lst !! (depth + delay)) == 0

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/13.txt")
  let rawStrs   = map (\x -> splitOn " " $ filter (not . (`elem` ":")) $ Text.unpack x) $ ls
      configs   = map (\x -> (read (head x) :: Int, read (head (tail x)) :: Int)) rawStrs
      maxDepth  = fst $ last configs
      cfgMap    = Map.fromList $ map (\(x, y) -> (x, (y, cycle ([0..y - 1] ++ reverse [1..(y - 2)])))) configs
      delay0Sev = severity maxDepth cfgMap 0
      minDelay  = head $ [x | x <- [0..], not $ snd $ severity maxDepth cfgMap x]
  print $ fst delay0Sev
  print minDelay
