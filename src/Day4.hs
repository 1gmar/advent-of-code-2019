module Day4
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.List (group)

inputRange :: (Int, Int)
inputRange = (134792, 675810)

generatePasswords :: (Int, Int) -> [Int -> Bool] -> [Int]
generatePasswords (lower, upper) criteria = [pass | pass <- [lower .. upper], all (\crit -> crit pass) criteria]

increasingDigits :: Int -> Bool
increasingDigits = and . snd . foldl currentDigitGTE ('0', []) . show
  where
    currentDigitGTE (prev, acc) current = (current, (current >= prev) : acc)

adjacentDuplicate :: (Int -> Bool) -> Int -> Bool
adjacentDuplicate criteria = any criteria . map length . group . show

solutionPart1 :: Int
solutionPart1 = (length . generatePasswords inputRange) [increasingDigits, adjacentDuplicate (> 1)]

solutionPart2 :: Int
solutionPart2 = (length . generatePasswords inputRange) [increasingDigits, adjacentDuplicate (== 2)]
