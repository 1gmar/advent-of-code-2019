module Day1
  ( solutionDay1Part1
  , solutionDay1Part2
  ) where

import           Data.Monoid (Sum (..))

evalMass :: Integer -> Integer
evalMass = subtract 2 . (`div` 3)

sumFuelMass :: [String] -> Integer
sumFuelMass = getSum . foldMap (Sum . evalMass . read)

sumTotalFuelMass :: [String] -> Integer
sumTotalFuelMass = getSum . foldMap Sum . fuelAwareMassValues
  where
    fuelAwareMassValues = concatMap (takeWhile (> 0) . iterate evalMass . evalMass . read)

solutionDay1Part1 :: IO Integer
solutionDay1Part1 = sumFuelMass . lines <$> readFile "./resources/input.txt"

solutionDay1Part2 :: IO Integer
solutionDay1Part2 = sumTotalFuelMass . lines <$> readFile "./resources/input.txt"
