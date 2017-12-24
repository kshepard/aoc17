module Day21 where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.List (foldl', nub, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

newtype Square = Square [String] deriving (Ord, Eq)
newtype Rule   = Rule (Square, Square)

data Config = Config Square (Map.Map Square Square)

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
  map (\x -> Rule (x, o)) . nub $ rotations ++ inversions
  where
    rotations  = take 4 . iterate rotSq $ i
    inversions = map revSq rotations

getRuleStrings :: [String] -> Reader Config [String]
getRuleStrings s = do
  (Config _ m) <- ask
  let (Square (newS)) = fromJust . Map.lookup (Square s) $ m
  return newS

enhance :: Square -> Reader Config Square
enhance (Square r) = do
  c <- ask
  let numChunks = if (even $ length r) then 2 else 3
      enhanced  = map concat
        . transpose
        . map (\x -> runReader (getRuleStrings x) c)
        . transpose
        . map (chunksOf numChunks)
        =<< chunksOf numChunks r
  return $ Square enhanced

enhanceTimes :: Int -> Reader Config Square
enhanceTimes x = do
  c@(Config i _) <- ask
  return . foldl' (\s _ -> runReader (enhance s) c) i $ [1..x]

countOnPixels :: Square -> Int
countOnPixels (Square s) = length . filter (`elem` "#") . concat $ s

main :: IO ()
main = do
  input <- readFile "input/21.txt"
  let rules    = concatMap expandRule . parseRules $ input
      rulesMap = Map.fromList . map (\(Rule x) -> x) $ rules
      startSq  = Square [".#.", "..#", "###"]
      runTimes = \x -> countOnPixels . runReader (enhanceTimes x) $ Config startSq rulesMap
  print $ runTimes 5
  print $ runTimes 18
