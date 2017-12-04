module Day04 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List.Split (splitOn)
import Data.List (nub, sort)

-- For a list of passphrases, returns 1 if
-- all elements are unique, 0 otherwise.
isUnique :: [String] -> Int
isUnique passes
  | length passes == (length $ nub passes) = 1
  | otherwise                              = 0

-- For a list of passphrases, returns 1 if
-- all elements are anagram-unique, 0 otherwise.
isAnagramUnique :: [String] -> Int
isAnagramUnique passes
  | length x == length y = 1
  | otherwise            = 0
  where
    x = map sort passes
    y = nub x

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/04.txt")
  let phrases = map (\x -> splitOn " " $ Text.unpack x) $ ls
  print $ sum $ map isUnique phrases
  print $ sum $ map isAnagramUnique phrases
