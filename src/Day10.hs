module Day10 where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (splitOn, chunksOf)
import Numeric (showHex)

knotRound :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
knotRound lens ilst cpos skip
  | lens == [] = (ilst, cpos, skip)
  | otherwise  = knotRound (tail lens) knotted newpos newskip
  where
    lilst    = length ilst
    cycilst  = cycle ilst
    clen     = head lens
    selected = take clen . drop cpos $ cycilst
    reversed = reverse selected
    newcyc   = cycle $ reversed ++ (take (lilst - clen) . drop (cpos + length selected) $ cycilst)
    knotted  = take lilst . drop (lilst - cpos) $ newcyc
    newpos   = (skip + clen + cpos) `mod` (length ilst)
    newskip  = skip + 1

runRounds :: [Int] -> [Int] -> Int -> [Int]
runRounds lens ilst numRounds = go ilst numRounds 0 0
  where
    go ilst' remaining c s
      | remaining == 0 = ilst'
      | otherwise      = go newlst (remaining - 1) cpos skip
      where
        (newlst, cpos, skip) = knotRound lens ilst' c s
main :: IO ()
main = do
  input <- readFile "input/10.txt"
  let str           = splitOn "," $ init input
      lens          = map (\x -> read x :: Int) str
      ilst          = [0..255] :: [Int]
      (p1lst, _, _) = knotRound lens ilst 0 0
      p1a : p1b : _ = p1lst
      suffixes      = [17, 31, 73, 47, 23] :: [Int]
      asciiIn       = map ord $ init input
      p2lens        = asciiIn ++ suffixes
      p2sparse      = runRounds p2lens ilst 64
      grouped       = chunksOf 16 p2sparse
      xored         = map (foldr (xor) 0) grouped
      hexes         = map (\x -> showHex x "") xored
      zeroPadded    = map (\x -> if length x == 2 then x else "0" ++ x) hexes
      hash          = foldr (++) "" zeroPadded
  print $ p1a * p1b
  print hash
