module Util.CyclicEnumClass (CyclicEnum (..)) where

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cPred :: a -> a
  cPred value
    | value == minBound = maxBound
    | otherwise = pred value
  cSucc :: a -> a
  cSucc value
    | value == maxBound = minBound
    | otherwise = succ value
