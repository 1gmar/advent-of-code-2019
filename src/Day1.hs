module Day1
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (isControl, isDigit)
import           Text.ParserCombinators.ReadP (ReadP, eof, munch1, readP_to_S, satisfy, sepBy, skipSpaces)

evalMass :: Int -> Int
evalMass = subtract 2 . (`div` 3)

sumFuelMass :: [Int] -> Int
sumFuelMass = sum . map evalMass

sumTotalFuelMass :: [Int] -> Int
sumTotalFuelMass = sum . fuelAwareMassValues
  where
    fuelAwareMassValues = concatMap (takeWhile (> 0) . iterate evalMass . evalMass)

inputParser :: ReadP [Int]
inputParser = skipSpaces *> line `sepBy` endOfLine <* skipSpaces <* eof
  where
    endOfLine = satisfy isControl
    line = read <$> munch1 isDigit

parseInput :: String -> [Int]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [Int]
readInput = parseInput <$> readFile "./resources/input-day1.txt"

solutionPart1 :: IO Int
solutionPart1 = sumFuelMass <$> readInput

solutionPart2 :: IO Int
solutionPart2 = sumTotalFuelMass <$> readInput
