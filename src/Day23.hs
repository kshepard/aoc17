module Day23 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map     as Map
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)

type TReg = String
type TVal = Int
type TPos = Int

data RegOrVal = Reg TReg | Val TVal deriving Show

data Instr = CSet TReg RegOrVal
           | CSub TReg RegOrVal
           | CMul TReg RegOrVal
           | CJnz RegOrVal RegOrVal
           deriving Show

readRegOrVal :: String -> RegOrVal
readRegOrVal s
  | iv     = Val (read s)
  | not iv = Reg s
  | otherwise = error $ "Bad input on readRegOrVal: " ++ s
  where
    iv = case reads s :: [(TVal, TReg)] of
      [(_, "")] -> True
      _         -> False

parse :: [String] -> Instr
parse ("set":x:y:_) = CSet x (readRegOrVal y)
parse ("sub":x:y:_) = CSub x (readRegOrVal y)
parse ("mul":x:y:_) = CMul x (readRegOrVal y)
parse ("jnz":x:y:_) = CJnz (readRegOrVal x) (readRegOrVal y)
parse _             = error $ "Bad input on parse"

getRV :: Map.Map TReg TVal -> RegOrVal -> TVal
getRV m (Reg r) = fromJust $ Map.lookup r m
getRV _ (Val v) = v

getR :: Map.Map TReg TVal -> TReg -> TVal
getR m r = fromJust $ Map.lookup r m

p1Exec :: [Instr] -> Map.Map TReg TVal -> TVal
p1Exec instrs mStart = go 0 0 mStart
  where
    go pos muls m
      | done      = muls
      | otherwise = go newPos newMuls newM
      where
        done  = pos >= length instrs
        instr = instrs !! pos
        (newM, newPos, newMuls) = case instr of
          CSet x y -> (Map.update (\_ -> Just (getRV m y)) x m, pos + 1, muls)
          CSub x y -> (Map.update (\_ -> Just (getR m x - getRV m y)) x m, pos + 1, muls)
          CMul x y -> (Map.update (\_ -> Just (getRV m y * getR m x)) x m, pos + 1, muls + 1)
          CJnz x y -> (m, if getRV m x /= 0 then pos + (getRV m y) else pos + 1, muls)

p2Exec :: [Instr] -> Map.Map TReg TVal -> TVal
p2Exec instrs mStart = go 0 mStart (0 :: Int)
  where
    go pos m s
      | done      = length . filter (not . isPrime) . map (+ (17 + b)) $ [0..1000]
      | otherwise = go newPos newM (s + 1)
      where
        b     = getR m "b"
        done  = s >= 10
        instr = instrs !! pos
        (newM, newPos) = case instr of
          CSet x y -> (Map.update (\_ -> Just (getRV m y)) x m, pos + 1)
          CSub x y -> (Map.update (\_ -> Just (getR m x - getRV m y)) x m, pos + 1)
          CMul x y -> (Map.update (\_ -> Just (getRV m y * getR m x)) x m, pos + 1)
          CJnz x y -> (m, if getRV m x <= 0 then pos + (getRV m y) else pos + 1)

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/23.txt")
  let raw     = map (\x -> splitOn " " $ Text.unpack x) $ ls
      instrs  = map parse raw
      mStart1 = Map.fromList $ map (\x -> ([x], 0)) ['a'..'h']
      mStart2 = Map.update (\_ -> Just 1) "a" mStart1
      part1   = p1Exec instrs mStart1
      part2   = p2Exec instrs mStart2
  print part1
  print part2
