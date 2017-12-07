module Day07 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split (splitOn)

data Config = Config { name   :: String,
                       weight :: Int,
                       cNames :: [String]
                     } deriving (Show)

-- Find the parent of a config
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

-- The root is the config with no parent
findRoot :: [Config] -> String
findRoot cs = go 0
  where
    go i
      | parent == Nothing = configName
      | otherwise         = go $ i + 1
      where
        configName = name $ cs !! i
        parent = findParent configName cs

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/07.txt")
  let rawStrs  = map (\x -> splitOn " " $ filter (not . (`elem` "()->,")) $ Text.unpack x) $ ls
      configs  = map (\x -> Config (x !! 0) (read (x !! 1)) (drop 3 x)) rawStrs
      rootName = findRoot configs
  print rootName
