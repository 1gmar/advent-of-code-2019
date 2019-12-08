module Day1
  ( solutionPart1
  , solutionPart2
  ) where

evalMass :: Integer -> Integer
evalMass = subtract 2 . (`div` 3)

sumFuelMass :: [Integer] -> Integer
sumFuelMass = sum . map evalMass

sumTotalFuelMass :: [Integer] -> Integer
sumTotalFuelMass = sum . fuelAwareMassValues
  where
    fuelAwareMassValues = concatMap (takeWhile (> 0) . iterate evalMass . evalMass)

readLines :: IO [Integer]
readLines = map read . lines <$> readFile "./resources/input-day1.txt"

solutionPart1 :: IO Integer
solutionPart1 = sumFuelMass <$> readLines

solutionPart2 :: IO Integer
solutionPart2 = sumTotalFuelMass <$> readLines
