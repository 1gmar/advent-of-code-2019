module Day1
  ( solutionDay1Part1
  , solutionDay1Part2
  ) where

import           Data.Monoid (Sum (..))

sumFuel :: [String] -> Integer
sumFuel = getSum . foldMap (Sum . computeModelFuel)
  where
    computeModelFuel = subtract 2 . (`div` 3) . read

sumTotalFuel :: [String] -> Integer
sumTotalFuel = getSum . foldMap Sum . computeModelTotalFuel
  where
    computeModelTotalFuel = concatMap (takeWhile (> 0) . iterate evaluateMass . evaluateMass . read)
    evaluateMass = subtract 2 . (`div` 3)

solutionDay1Part1 :: IO Integer
solutionDay1Part1 = sumFuel . lines <$> readFile "./resources/input.txt"

solutionDay1Part2 :: IO Integer
solutionDay1Part2 = sumTotalFuel . lines <$> readFile "./resources/input.txt"
