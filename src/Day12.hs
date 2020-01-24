{-# LANGUAGE RecordWildCards #-}

module Day12
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Foldable (foldl')
import           Data.List     (findIndex, foldl1', transpose)
import           ParseUtils

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

buildMoonData :: (Int, Int, Int) -> Moon
buildMoonData (x, y, z) = [spaceD x, spaceD y, spaceD z]
  where
    spaceD pos = SpaceD pos 0

inputParser :: ReadP [Moon]
inputParser = trimSpacesEOF $ count 4 (line <* endOfLine)
  where
    line = buildMoonData <$> positionTuple
    positionTuple = (,,) <$> x <*> y <*> z
    x = string "<x=" *> integer <* char ',' <* skipSpaces
    y = string "y=" *> integer <* char ',' <* skipSpaces
    z = string "z=" *> integer <* char '>'

solutionPart1 :: Int -> String -> Int
solutionPart1 steps = computeTotalEnergy . stepNTimes steps . parseInput inputParser

solutionPart2 :: String -> Maybe Int
solutionPart2 = findStepsForMoonCycle . parseInput inputParser
