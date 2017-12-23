module Day22 where

import Data.List (foldl')
import qualified Data.Map as Map

type RC     = (Int, Int)
type Grid   = Map.Map RC Char
type BState = (Grid, RC, Dir, Int)

data Dir = N | S | E | W deriving (Eq, Show)

turnRight :: Dir -> Dir
turnRight N = E
turnRight S = W
turnRight E = S
turnRight W = N

turnLeft :: Dir -> Dir
turnLeft N = W
turnLeft S = E
turnLeft E = N
turnLeft W = S

turnAround :: Dir -> Dir
turnAround N = S
turnAround S = N
turnAround E = W
turnAround W = E

parseGrid :: String -> Grid
parseGrid input = Map.fromList g
  where
    l = lines input
    n = (length l - 1) `div` 2
    g = zip [ (r, c) | r <- [-n..n], c <- [-n..n] ] $ concat l

getStatus :: Grid -> RC -> Char
getStatus g rc =
  case Map.lookup rc g of
    Nothing -> '.'
    Just x  -> x

setStatus :: Grid -> RC -> Char -> Grid
setStatus g rc v =
  case Map.lookup rc g of
    Nothing -> Map.insert rc v g
    Just _  -> Map.adjust (\_ -> v) rc g

nextPos :: RC -> Dir -> RC
nextPos (r, c) N = (r - 1, c)
nextPos (r, c) S = (r + 1, c)
nextPos (r, c) E = (r, c + 1)
nextPos (r, c) W = (r, c - 1)

burst1 :: BState -> BState
burst1 (grid, cpos, cdir, numInfected) = (grid', cpos', cdir', numInfected')
  where
    status       = getStatus grid cpos
    infected     = status == '#'
    cdir'        = if infected then turnRight cdir else turnLeft cdir
    grid'        = setStatus grid cpos (if infected then '.' else '#')
    numInfected' = if infected then numInfected else numInfected + 1
    cpos'        = nextPos cpos cdir'

burst2 :: BState -> BState
burst2 (grid, cpos, cdir, numInfected) = (grid', cpos', cdir', numInfected')
  where
    status       = getStatus grid cpos
    cdir'        = case status of
                     '.' -> turnLeft cdir
                     'W' -> cdir
                     '#' -> turnRight cdir
                     _   -> turnAround cdir
    newStatus    = case status of
                     '.' -> 'W'
                     'W' -> '#'
                     '#' -> 'F'
                     _   -> '.'
    grid'        = setStatus grid cpos newStatus
    numInfected' = if status == 'W' then numInfected + 1 else numInfected
    cpos'        = nextPos cpos cdir'

main :: IO ()
main = do
  input <- readFile "input/22.txt"
  let grid          = parseGrid input
      (_, _, _, p1) = iterate burst1 (grid, (0, 0), N, 0) !! 10000
      (_, _, _, p2) = iterate burst2 (grid, (0, 0), N, 0) !! 10000000
  print p1
  print p2
