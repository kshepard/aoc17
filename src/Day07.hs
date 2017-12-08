module Day07 where

import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Config = Config { name   :: String,
                       weight :: Int,
                       cNames :: [String]
                     } deriving (Show)

-- Finds the parent of a config
findParent :: String -> [Config] -> Maybe String
findParent cn cs = go 0
  where
    lencs = length cs
    go i
      | i >= lencs       = Nothing
      | elem cn children = Just pcn
      | otherwise        = go $ i + 1
      where
        pc       = cs !! i
        pcn      = name pc
        children = cNames pc

-- Finds the root config (the config with no parent)
findRoot :: [Config] -> String
findRoot cs = go 0
  where
    go i
      | parent == Nothing = configName
      | otherwise         = go $ i + 1
      where
        configName = name $ cs !! i
        parent     = findParent configName cs

-- Calculates the weight of a config's children
childWeight :: String -> Map.Map String Config -> Bool -> Int
childWeight cn cm fi
  | length children == 0 = extra
  | otherwise            = extra + (sum $ map (\x -> childWeight x cm False) children)
  where
    cfg      = fromJust $ Map.lookup cn cm
    children = cNames cfg
    extra    = if fi then 0 else weight cfg

-- Finds the children [(name, weight)] with the incorrect weight
findBadWeight :: [Config] -> Map.Map String Config -> [(String, Int)]
findBadWeight cs cm = go 0
  where
    go i
      | length children == 0 || not isBad = go $ i + 1
      | otherwise                         = zip children cws
      where
        cfg      = cs !! i
        children = cNames cfg
        cws      = map (\x -> weight (fromJust $ Map.lookup x cm)
                         + (childWeight x cm True)) children
        isBad    = length children <= 5 && length (nub cws) == 2

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/07.txt")
  let rawStrs    = map (\x -> splitOn " " $ filter (not . (`elem` "()->,")) $ Text.unpack x) $ ls
      configs    = map (\x -> Config (x !! 0) (read (x !! 1)) (drop 3 x)) rawStrs
      cfgMap     = Map.fromList $ map (\x -> (name x, x)) configs
      badWeights = findBadWeight configs cfgMap
      -- TODO: need to finish up the logic here to make it work for all cases.
      -- Currently expects the "different" weight to be last in the returned
      -- badWeights list, which isn't guaranteed, but was easy to notice with a spot check.
      badOne     = last badWeights
      badWeight  = snd badOne
      goodWeight = snd $ head badWeights
      badCfg     = fromJust $ Map.lookup (fst badOne) cfgMap
  print $ findRoot configs
  print $ (weight badCfg) - (badWeight - goodWeight)
