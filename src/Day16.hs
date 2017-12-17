module Day16 where

import Data.List.Split (splitOn)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving Show
type Prog = Char

parse :: String -> Move
parse ('s':count) = Spin $ read count
parse ('x':rest)  = (\[a,b] -> Exchange (read a) (read b)) $ splitOn "/" rest
parse ('p':rest)  = (\[a,b] -> Partner (a !! 0) (b !! 0)) $ splitOn "/" rest
parse _           = error "Bad input"

move :: Move -> Seq.Seq Prog -> Seq.Seq Prog

move (Spin x) progs = (Seq.><) (Seq.drop len progs) (Seq.take len progs)
  where
    len = Seq.length progs - x

move (Exchange a b) progs = Seq.update b av $ Seq.update a bv progs
  where
    av = Seq.index progs a
    bv = Seq.index progs b

move (Partner a b) progs = Seq.update bi a $ Seq.update ai b progs
  where
    ai = fromJust $ Seq.elemIndexL a progs
    bi = fromJust $ Seq.elemIndexL b progs

dance :: [Move] -> Seq.Seq Prog -> Seq.Seq Prog
dance (m:ms) ps = dance ms $ move m ps
dance _ ps      = ps

danceTimes :: Int -> Map.Map (Seq.Seq Prog) (Seq.Seq Prog) -> [Move] -> Seq.Seq Prog -> Seq.Seq Prog
danceTimes 0 _ _ ps  = ps
danceTimes n c ms ps = danceTimes remaining newCache ms result
  where
    cached     = Map.lookup ps c
    calculated = dance ms ps
    result     = if cached == Nothing then calculated else fromJust cached
    newCache   = if cached == Nothing then Map.insert ps calculated c else c
    remaining  = if cached == Nothing then n - 1 else n `mod` (1000000000 - n) - 1

main :: IO ()
main = do
  input <- readFile "input/16.txt"
  let strs  = splitOn "," $ init input
      moves = map parse strs
      progs = Seq.fromList ['a'..'p']
  print $ toList $ dance moves progs
  print $ toList $ danceTimes 1000000000 Map.empty moves progs
