module Day12 where

import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

expand :: [String] -> Map.Map String [String] -> [String]
expand froms m = nub $ concat((map getTos froms)) ++ froms
  where getTos from = fromJust $ Map.lookup from m

connections :: String -> Map.Map String [String] -> [String]
connections from m = go [from] []
  where
    go acc old
      | length acc == length old = acc
      | otherwise                = go (expand acc m) acc

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/12.txt")
  let rawStrs = map (\x -> splitOn " " $ filter (not . (`elem` "<->,")) $ Text.unpack x) $ ls
      configs = map (\x -> ((head x), (drop 2 x))) rawStrs
      froms   = map fst configs
      cfgMap  = Map.fromList configs
      conns   = map (\x -> connections x cfgMap) froms
  print $ length $ filter (elem "0") conns
  print $ length $ nub $ map sort conns
