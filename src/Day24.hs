module Day24 where

import Data.List (delete, foldl', nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Component = (Int, Int)
type Bridge    = [Component]

parseComponents :: String -> [Component]
parseComponents = map (\x -> (read $ x !! 0, read $ x !! 1)) . map (splitOn "/") . lines

maybeAddComponent :: Component -> Bridge -> Maybe Bridge
maybeAddComponent c b = mbridge
  where
    validF = b /= [] && fst c == snd (last b)
    validB = b /= [] && snd c == snd (last b)
    initF  = b == [] && fst c == 0
    initB  = b == [] && snd c == 0
    mbridge
      | initF || validF = Just (b ++ [c])
      | initB || validB = Just (b ++ [(snd c, fst c)])
      | otherwise       = Nothing

mkBridges :: Component -> [Component] -> Bridge -> [Bridge]
mkBridges c cs b
  | mb == Nothing = [b]
  | otherwise     = concatMap (\c' -> mkBridges c' (delete c' cs) b') cs
  where
    mb = maybeAddComponent c b
    b' = fromJust mb

isUseful :: Component -> [Component] -> Bool
isUseful c@(x, y) cs = elem x nums || elem y nums
  where
    cs'  = delete c cs
    nums = nub . concatMap (\(x, y) -> [x, y]) $ cs'

main :: IO ()
main = do
  input <- readFile "input/24.txt"
  let tcs = parseComponents input
      cs  = filter (\c -> isUseful c tcs) tcs
      bs  = concatMap (\c -> mkBridges c (delete c cs) []) cs
      p1  = maximum . map (\cs' -> foldl' (\a c -> a + fst c + snd c) 0 cs') $ bs
      p2  = snd . maximum . map (\cs' -> (length cs', foldl' (\a c -> a + fst c + snd c) 0 cs')) $ bs
  print p1
  print p2
