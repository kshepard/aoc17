module Day17 where

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Sequence as Seq

type StepTup = (Int, Seq.Seq Int)

next :: Int -> Int -> StepTup -> StepTup
next steps num (pos, buffer) = (newPos, newBuffer)
  where
    newPos    = 1 + (pos + steps) `mod` num
    newBuffer = Seq.insertAt newPos num buffer

main :: IO ()
main = do
  f <- readFile "input/17.txt"
  let steps           = read f :: Int
      spinlock        = foldl' (\acc n -> next steps n acc) (0, Seq.fromList [0])
      (pos1, buffer1) = spinlock [1..2017]
      (_, buffer2)    = spinlock [1..50000000]
  print $ Seq.index buffer1 (pos1 + 1)
  print $ Seq.index buffer2 (1 + (fromJust $ Seq.elemIndexL 0 buffer2))
