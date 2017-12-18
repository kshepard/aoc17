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

p1Exec :: [Instr] -> TVal
p1Exec instrs = go 0 0 mStart
  where
    mStart = Map.fromList $ map (\x -> ([x], 0)) ['a'..'z']
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

main :: IO ()
main = do
  ls <- fmap Text.lines (Text.readFile "input/18.txt")
  let raw    = map (\x -> splitOn " " $ Text.unpack x) $ ls
      instrs = map parse raw
      part1  = p1Exec instrs
  print part1
