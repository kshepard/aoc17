module Day05 where

parse :: [String] -> [Int]
parse = map read

maze :: [Int] -> (Int -> Int) -> Int
maze ints getVal = go 0 0 ints
  where
    go ind steps ints'
      | ind < 0            = steps
      | ind >= length ints = steps
      | otherwise          = go newInd (steps + 1) newInts
      where
        currVal = ints' !! ind
        newInd  = ind + currVal
        newVal  = getVal currVal
        newInts = take ind ints' ++ [newVal] ++ drop (ind + 1) ints'

main :: IO ()
main = do
  ints <- parse . lines <$> readFile "input/05.txt"
  print $ maze ints (+1)
  print $ maze ints (\x -> if x >= 3 then x - 1 else x + 1)
