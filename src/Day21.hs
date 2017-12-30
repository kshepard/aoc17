{-# LANGUAGE FlexibleContexts #-}
module Day21 where

import Control.Monad.Trans
import Control.Monad.Reader

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

enhance :: MonadReader Config m => Square -> m (Square)
enhance (Square r) = do
  Config _ m <- ask
  let getRuleStrings s = newS
        where
          Square (newS) = fromJust . Map.lookup (Square s) $ m
      numChunks = if even $ length r then 2 else 3
      enhanced  = map concat
        . transpose
        . map getRuleStrings
        . transpose
        . map (chunksOf numChunks)
        =<< chunksOf numChunks r
  return (Square enhanced)

enhanceTimes :: MonadReader Config m => Int -> m (Square)
enhanceTimes x = do
  Config i _ <- ask
  foldM (\s _ -> enhance s) i [1..x]

countOnPixels :: Square -> Int
countOnPixels (Square s) = length . filter (`elem` "#") . concat $ s

parseConfig :: MonadIO m => String -> m (Config)
parseConfig f = do
  input <- liftIO . readFile $ f
  let rules    = concatMap expandRule . parseRules $ input
      rulesMap = Map.fromList . map (\(Rule x) -> x) $ rules
      startSq  = Square [".#.", "..#", "###"]
      config   = Config startSq rulesMap
  return config

solve :: (MonadReader Config m, MonadIO m) => [Int] -> m ()
solve = mapM_ (\x -> enhanceTimes x >>= liftIO . print . countOnPixels)

main :: IO ()
main = parseConfig "input/21.txt" >>= runReaderT (solve [5, 18])
