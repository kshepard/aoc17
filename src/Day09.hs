module Day09 where

sln :: String -> (Int, Int)
sln s = go 0 s False 0 0
  where
    go level (c:cs) garb acc gacc
      | cs == []         = (acc, gacc)
      | c == '!'         = go level (tail cs) garb acc gacc
      | garb && c == '>' = go level cs False acc gacc
      | garb             = go level cs True acc (gacc + 1)
      | c == '{'         = go (level + 1) cs garb (acc + level + 1) gacc
      | c == '}'         = go (level - 1) cs garb acc gacc
      | c == '<'         = go level cs True acc gacc
      | otherwise        = go level cs garb acc gacc

main :: IO ()
main = do
  s <- readFile "input/09.txt"
  let (p1, p2) = sln $ init s
  print p1
  print p2
