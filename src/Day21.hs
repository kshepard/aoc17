module Day21 where

import Data.List (foldl', nub, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

newtype Square = Square [String] deriving (Ord, Eq)
newtype Rule   = Rule (Square, Square)

mkRule :: [Square] -> Rule
mkRule sqs = Rule (sqs !! 0, sqs !! 1)

parseSquare :: String -> Square
parseSquare = Square . splitOn "/"

parseRule :: String -> Rule
parseRule = mkRule . (map parseSquare) . splitOn " => "

parseRules :: String -> [Rule]
parseRules = map parseRule . lines

revSq :: Square -> Square
revSq (Square s) = Square $ reverse s

transposeSq :: Square -> Square
transposeSq (Square s) = Square $ transpose s

rotSq :: Square -> Square
rotSq = revSq . transposeSq

expandRule :: Rule -> [Rule]
expandRule (Rule (i, o)) =
  map (\x -> Rule (x, o)) $ nub $ rotations ++ inversions
  where
    rotations  = take 4 . iterate rotSq $ i
    inversions = map revSq rotations

getRuleStrings :: [String] -> Map.Map Square Square -> [String]
getRuleStrings s m = newS
  where
    (Square (newS)) = fromJust $ Map.lookup (Square s) m

enhance :: Square -> Map.Map Square Square -> Square
enhance (Square r) m = Square enhanced
  where
    numChunks = if (even $ length r) then 2 else 3
    enhanced  = map concat
      . transpose
      . (map (\x -> getRuleStrings x m))
      . transpose
      . map (chunksOf numChunks)
      =<< chunksOf numChunks r

enhanceTimes :: Square -> Map.Map Square Square -> Int -> Square
enhanceTimes i m x = foldl' (\s _ -> enhance s m) i [1..x]

countOnPixels :: Square -> Int
countOnPixels (Square s) = length . filter (`elem` "#") . concat $ s

main :: IO ()
main = do
  input <- readFile "input/21.txt"
  let rules    = concatMap expandRule . parseRules $ input
      rulesMap = Map.fromList $ map (\(Rule (i, o)) -> (i, o)) $ rules
      startSq  = Square [".#.", "..#", "###"]
  print $ countOnPixels $ enhanceTimes startSq rulesMap 5
  print $ countOnPixels $ enhanceTimes startSq rulesMap 18
