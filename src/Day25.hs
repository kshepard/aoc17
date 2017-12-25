module Day25 where

import qualified Data.Map as Map
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust)

data Dir     = R | L deriving (Eq, Ord, Show)
data Val     = Zero | One deriving (Eq, Ord, Show)
type Pos     = Int
type Steps   = Int
type State   = Char
type SV      = (State, Val)
type Instr   = (State, Val, Val, Dir, State)
type BPrint  = (State, Steps, [Instr])
type InstrsM = Map.Map SV Instr
type TapeM   = Map.Map Pos Val
type StepTup = (State, Pos, TapeM)

parseInstrs :: [String] -> [Instr]
parseInstrs ls = [(cs, Zero, nv0, d0, ns0), (cs, One, nv1, d1, ns1)]
  where
    cs  = (ls !! 0) !! 9
    nv0 = if (ls !! 2) !! 22 == '1' then One else Zero
    d0  = if drop 27 (ls !! 3) == "right." then R else L
    ns0 = (ls !! 4) !! 26
    nv1 = if (ls !! 6) !! 22 == '1' then One else Zero
    d1  = if drop 27 (ls !! 7) == "right." then R else L
    ns1 = (ls !! 8) !! 26

parseBlueprint :: String -> BPrint
parseBlueprint input = (state, steps, instrs)
  where
    ls     = lines input
    state  = (ls !! 0) !! 15
    steps  = read . head . splitOn " " . drop 36 $ ls !! 1
    instrs = concatMap parseInstrs . chunksOf 10 . drop 3 $ ls

getVal :: TapeM -> Pos -> Val
getVal t p =
  case Map.lookup p t of
    Nothing -> Zero
    Just x  -> x

setVal :: TapeM -> Pos -> Val -> TapeM
setVal t p v =
  case Map.lookup p t of
    Nothing -> Map.insert p v t
    Just _  -> Map.adjust (\_ -> v) p t

runStep :: InstrsM -> StepTup -> StepTup
runStep im (s, p, t) = (s', p', t')
  where
    currV             = getVal t p
    (_, _, nv, d, s') = fromJust $ Map.lookup (s, currV) im
    t'                = setVal t p nv
    p'                = if d == R then p + 1 else p - 1

checkSum :: TapeM -> Int
checkSum = Map.foldr (\n acc -> acc + (if n == One then 1 else 0)) 0

main :: IO ()
main = do
  input <- readFile "input/25.txt"
  let (start, steps, instrs)
                   = parseBlueprint input
      im           = Map.fromList $ map (\i@(s, v, _, _, _ ) -> ((s, v), i)) instrs
      (_, _, tape) = foldl' (\acc _ -> runStep im acc) (start, 0, Map.empty) [1..steps]
  print $ checkSum tape
