module Day08 where

import qualified Data.Map     as Map
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Jump = Jump { reg1 :: String,
                   op :: String,
                   amt1 :: Int,
                   reg2 :: String,
                   cond :: String,
                   amt2 :: Int
                 } deriving (Show)

isTrue :: Map.Map String Int -> Jump -> Bool
isTrue regMap jump
  | cond jump == ">"  && x >  amt2 jump = True
  | cond jump == "<"  && x <  amt2 jump = True
  | cond jump == "<=" && x <= amt2 jump = True
  | cond jump == ">=" && x >= amt2 jump = True
  | cond jump == "!=" && x /= amt2 jump = True
  | cond jump == "==" && x == amt2 jump = True
  | otherwise                           = False
  where
    x = fromJust $ Map.lookup (reg2 jump) regMap

updateMap :: Map.Map String Int -> Jump -> Map.Map String Int
updateMap regMap jump = Map.adjust updateFn (reg1 jump) regMap
  where
    operation
      | op jump == "inc"   = (+)
      | otherwise          = (subtract)
    updateFn
      | isTrue regMap jump = operation (amt1 jump)
      | otherwise          = (+ 0)

execJumps :: [Jump] -> Map.Map String Int -> (Map.Map String Int, Int)
execJumps jumps regMap = go 0 regMap 0
  where
    go i rm p2
      | i >= length jumps = (rm, p2)
      | otherwise         = go (i + 1) updated (maximum [maxEncountered, p2])
      where
        updated           = updateMap rm (jumps !! i)
        maxEncountered    = maximum $ map snd $ Map.toList updated


main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/08.txt")
  let rawStrs        = map (\x -> splitOn " " $ Text.unpack x) $ ls
      jumps          = map (\x -> Jump (x !! 0) (x !! 1) (read (x !! 2))
                             (x !! 4) (x !! 5) (read (x !! 6))) rawStrs
      regMap         = Map.fromList $ map (\x -> (x, 0)) $ nub $ map reg1 jumps
      (finalMap, p2) = execJumps jumps regMap

  print $ maximum $ map snd $ Map.toList finalMap
  print p2
