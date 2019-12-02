module Day1
  ( solutionPart1
  , solutionPart2
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

readLines :: IO [String]
readLines = lines <$> readFile "./resources/input-day1.txt"

solutionPart1 :: IO Integer
solutionPart1 = sumFuelMass <$> readLines

solutionPart2 :: IO Integer
solutionPart2 = sumTotalFuelMass <$> readLines
