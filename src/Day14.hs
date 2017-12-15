module Day14 where

import Day10 (knotHash)
import qualified Data.Set as Set

hexToBin :: String -> String
hexToBin = concat . map hexCharToBin
  where
    hexCharToBin c
      | c == '0'  = "0000"
      | c == '1'  = "0001"
      | c == '2'  = "0010"
      | c == '3'  = "0011"
      | c == '4'  = "0100"
      | c == '5'  = "0101"
      | c == '6'  = "0110"
      | c == '7'  = "0111"
      | c == '8'  = "1000"
      | c == '9'  = "1001"
      | c == 'a'  = "1010"
      | c == 'b'  = "1011"
      | c == 'c'  = "1100"
      | c == 'd'  = "1101"
      | c == 'e'  = "1110"
      | c == 'f'  = "1111"
      | otherwise = error $ "Unsupported: " :: c

mkCoordsSet :: [String] -> Set.Set (Int, Int)
mkCoordsSet brs = Set.fromList $ map (\(_, x, y) -> (x, y)) filtered
  where
    withYs   = zip [0..(length brs)] brs
    withXs   = map (\(x, y) -> (x, zip [0..(length y)] y)) $ withYs
    tups     = concat [ map (\(c, b) -> (b, c, r)) ts | (r, ts) <- withXs ]
    filtered = filter (\(t, _, _) -> t == '1') tups

expand :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
expand subset s = Set.intersection s neighbors
  where
    neighbors = Set.fromList
      $ concat
      $ map (\(x, y) -> [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
      $ Set.toList subset

findAdjacents :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
findAdjacents s (x, y) = go (Set.fromList [(x, y)]) Set.empty
  where
    go acc old
      | Set.size acc == Set.size old = acc
      | otherwise                    = go (expand acc s) acc

countRegions :: Set.Set (Int, Int) -> Int
countRegions s = go s 0
  where
    go s' count
      | Set.size s' == 0 = count
      | otherwise        = go newSet (count + 1)
      where
        adjacents = findAdjacents s' $ Set.elemAt 0 s'
        newSet    = Set.difference s' adjacents

main :: IO ()
main = do
  f <- readFile "input/14.txt"
  let input     = init f
      inputs    = map (\x -> input ++ "-" ++ show x) ([0..127] :: [Int])
      hashes    = map knotHash inputs
      binRows   = map hexToBin hashes
      numUsed   = sum $ map (\x -> length $ filter (== '1') x) binRows
      coordsSet = mkCoordsSet binRows
  print numUsed
  print $ countRegions coordsSet
