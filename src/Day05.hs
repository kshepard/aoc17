module Day05 where

-- Note: ghci can't take advantage of the fast vectors.
-- To run fully optimized, do something like the following instead:
-- runghc --ghc-arg=-fobject-code --ghc-arg=-O2 src/Day05.hs

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

maze :: V.Vector Int -> (Int -> Int) -> Int
maze ints getVal = runST $ V.thaw ints >>= go 0 0
  where
    go ind steps v
      | ind < 0 || ind >= M.length v = return steps
      | otherwise = do
          currVal <- M.read v ind
          M.write v ind $ getVal currVal
          go (ind + currVal) (steps + 1) v

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input/05.txt"
  let ints = V.fromList input
  print $ maze ints (+1)
  print $ maze ints (\x -> if x >= 3 then x - 1 else x + 1)
