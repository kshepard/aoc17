module Day18 where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Map     as Map
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type TReg = String
type TVal = Int
type TPos = Int

data RegOrVal = Reg TReg | Val TVal deriving Show

data Instr = CSnd RegOrVal
           | CSet TReg RegOrVal
           | CAdd TReg RegOrVal
           | CMul TReg RegOrVal
           | CMod TReg RegOrVal
           | CRcv TReg
           | CJgz RegOrVal RegOrVal
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
parse ("snd":x:_)   = CSnd (readRegOrVal x)
parse ("set":x:y:_) = CSet x (readRegOrVal y)
parse ("add":x:y:_) = CAdd x (readRegOrVal y)
parse ("mul":x:y:_) = CMul x (readRegOrVal y)
parse ("mod":x:y:_) = CMod x (readRegOrVal y)
parse ("rcv":x:_)   = CRcv x
parse ("jgz":x:y:_) = CJgz (readRegOrVal x) (readRegOrVal y)
parse _             = error $ "Bad input on parse"

getRV :: Map.Map TReg TVal -> RegOrVal -> TVal
getRV m (Reg r) = fromJust $ Map.lookup r m
getRV _ (Val v) = v

getR :: Map.Map TReg TVal -> TReg -> TVal
getR m r = fromJust $ Map.lookup r m

p1Exec :: [Instr] -> Map.Map TReg TVal -> TVal
p1Exec instrs mStart = go 0 0 mStart
  where
    go pos freq m
      | done      = freq
      | otherwise = go newPos newFreq newM
      where
        instr = instrs !! pos
        (newM, newPos, newFreq, done) = case instr of
          CSnd x   -> (m, pos + 1, getRV m x, False)
          CSet x y -> (Map.update (\_ -> Just (getRV m y)) x m, pos + 1, freq, False)
          CAdd x y -> (Map.update (\_ -> Just (getRV m y + getR m x)) x m, pos + 1, freq, False)
          CMul x y -> (Map.update (\_ -> Just (getRV m y * getR m x)) x m, pos + 1, freq, False)
          CMod x y -> (Map.update (\_ -> Just (getR m x `mod` getRV m y)) x m, pos + 1, freq, False)
          CRcv x   -> (m, pos + 1, freq, if getR m x > 0 then True else False)
          CJgz x y -> (m, if getRV m x > 0 then pos + (getRV m y) else pos + 1, freq, False)

p2Exec :: [Instr] -> Map.Map TReg TVal -> Map.Map TReg TVal -> Int
p2Exec instrs mStart0 mStart1 = go 0 0 mStart0 mStart1 [] [] 0 0 0
  where
    go pos0 pos1 m0 m1 q0 q1 execP p1SendCount waitCount
      | done      = p1SendCount
      | otherwise = go newPos0 newPos1 newM0 newM1 newQ0 newQ1 newExecP newP1SendCount newWaitCount
      where
        m              = if execP == 0 then m0 else m1
        q              = if execP == 0 then q0 else q1
        oq             = if execP == 0 then q1 else q0
        pos            = if execP == 0 then pos0 else pos1
        instr          = instrs !! pos
        newPos0        = if execP == 0 then newPos else pos0
        newPos1        = if execP == 1 then newPos else pos1
        newM0          = if execP == 0 then newM else m0
        newM1          = if execP == 1 then newM else m1
        newQ0          = if execP == 0 then newQ else newOQ
        newQ1          = if execP == 1 then newQ else newOQ
        newExecP       = if receiving then (execP + 1) `mod` 2 else execP
        newP1SendCount = if sending && execP == 1 then (p1SendCount + 1) else p1SendCount
        newWaitCount   = if receiving then waitCount + 1 else 0
        done           = newWaitCount >= 2
        (newM, newPos, newQ, newOQ, receiving, sending) = case instr of
          CSnd x   -> (m, pos + 1, q, oq ++ [getRV m x], False, True)
          CSet x y -> (Map.update (\_ -> Just (getRV m y)) x m, pos + 1, q, oq, False, False)
          CAdd x y -> (Map.update (\_ -> Just (getRV m y + getR m x)) x m, pos + 1, q, oq, False, False)
          CMul x y -> (Map.update (\_ -> Just (getRV m y * getR m x)) x m, pos + 1, q, oq, False, False)
          CMod x y -> (Map.update (\_ -> Just (getR m x `mod` getRV m y)) x m, pos + 1, q, oq, False, False)
          CRcv x   -> if q == []
                      then (m, pos, q, oq, True, False)
                      else (Map.update (\_ -> Just (head q)) x m, pos + 1, tail q, oq, False, False)
          CJgz x y -> (m, if getRV m x > 0 then pos + (getRV m y) else pos + 1, q, oq, False, False)

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/18.txt")
  let raw     = map (\x -> splitOn " " $ Text.unpack x) $ ls
      instrs  = map parse raw
      mStart0 = Map.fromList $ map (\x -> ([x], 0)) ['a'..'z']
      mStart1 = Map.update (\_ -> Just 1) "p" mStart0
      part1   = p1Exec instrs mStart0
      part2   = p2Exec instrs mStart0 mStart1
  print part1
  print part2
