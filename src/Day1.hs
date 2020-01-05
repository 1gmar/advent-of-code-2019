module Day1
  ( solutionPart1
  , solutionPart2
  ) where

import           ParseUtils

evalMass :: Int -> Int
evalMass = subtract 2 . (`div` 3)

sumFuelMass :: [Int] -> Int
sumFuelMass = sum . map evalMass

sumTotalFuelMass :: [Int] -> Int
sumTotalFuelMass = sum . fuelAwareMassValues
  where
    fuelAwareMassValues = concatMap (takeWhile (> 0) . iterate evalMass . evalMass)

inputParser :: ReadP [Int]
inputParser = trimSpacesEOF $ integer `sepBy` endOfLine

parseInput :: String -> [Int]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [Int]
readInput = parseInput <$> readFile "./resources/input-day1.txt"

solutionPart1 :: IO Int
solutionPart1 = sumFuelMass <$> readInput

solutionPart2 :: IO Int
solutionPart2 = sumTotalFuelMass <$> readInput
