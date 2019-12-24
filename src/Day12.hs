{-# LANGUAGE RecordWildCards #-}

module Day12 where

import           Data.Char                    (isControl, isDigit)
import           Data.Foldable                (foldl')
import           Data.List                    (findIndex, foldl1', transpose)
import           Text.ParserCombinators.ReadP (ReadP, char, count, eof, munch, munch1, readP_to_S, skipSpaces, string,
                                               (+++))

data SpaceD =
  SpaceD
    { x  :: !Int
    , dx :: !Int
    }
  deriving (Show)

type Moon = [SpaceD]

applyGravity :: SpaceD -> SpaceD -> SpaceD
applyGravity refD@(SpaceD x dx) (SpaceD x2 _) = refD {dx = dx + ddx}
  where
    ddx =
      case x `compare` x2 of
        LT -> 1
        GT -> -1
        EQ -> 0

moveOnAxis :: SpaceD -> SpaceD
moveOnAxis spaceD@SpaceD {..} = spaceD {x = x + dx}

axisTimeStep :: [SpaceD] -> [SpaceD]
axisTimeStep axisDs = map (moveOnAxis . applyAxisGravity) axisDs
  where
    applyAxisGravity axisD = foldl' applyGravity axisD axisDs

stepNTimes :: Int -> [Moon] -> [Moon]
stepNTimes n moons = transpose $ map ((!! n) . iterate axisTimeStep) moonsPerAxis
  where
    moonsPerAxis = transpose moons

computeTotalEnergy :: [Moon] -> Int
computeTotalEnergy = sum . map computeMoonEnergy
  where
    computeMoonEnergy moon = sum (abs . x <$> moon) * sum (abs . dx <$> moon)

findStepsForMoonCycle :: [Moon] -> Maybe Int
findStepsForMoonCycle moons = foldl1' lcm <$> stepsPerAxis
  where
    stepsPerAxis = traverse (countSteps . tail . iterate axisTimeStep) moonsPerAxis
    countSteps = fmap ((* 2) . (+ 1)) . stepsToInvertedDx
    stepsToInvertedDx = findIndex (all (== 0) . fmap dx)
    moonsPerAxis = transpose moons

buildMoonData :: (String, String, String) -> Moon
buildMoonData (x, y, z) = [spaceD x, spaceD y, spaceD z]
  where
    spaceD pos = read pos `SpaceD` 0

inputParser :: ReadP [Moon]
inputParser = skipSpaces *> dataLines <* skipSpaces <* eof
  where
    dataLines = count 4 (line <* endOfLine)
    endOfLine = munch isControl
    line = buildMoonData <$> positionTuple
    positionTuple = (,,) <$> x <*> y <*> z
    x = string "<x=" *> integer <* char ',' <* skipSpaces
    y = string "y=" *> integer <* char ',' <* skipSpaces
    z = string "z=" *> integer <* char '>'
    integer = positive +++ negative
    positive = munch1 isDigit
    negative = char '-' >>= \sign -> (sign :) <$> positive

parseInput :: String -> [Moon]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [Moon]
readInput = parseInput <$> readFile "./resources/input-day12.txt"

solutionPart1 :: IO Int
solutionPart1 = computeTotalEnergy . stepNTimes 1000 <$> readInput

solutionPart2 :: IO (Maybe Int)
solutionPart2 = findStepsForMoonCycle <$> readInput
