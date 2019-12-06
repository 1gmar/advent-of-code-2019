{-# LANGUAGE RecordWildCards #-}

module Day3
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (isDigit)
import           Text.ParserCombinators.ReadP (ReadP, char, choice, eof, munch, readP_to_S, sepBy, skipSpaces)

data SegmentType
  = Vertical
  | Horizontal

data Segment
  = VSegment
      { x      :: Int
      , yRange :: (Int, Int)
      , vSteps :: Int
      }
  | HSegment
      { y      :: Int
      , xRange :: (Int, Int)
      , hSteps :: Int
      }

type Line = [Segment]

type Point = (Int, Int)

type StepsCache = [(Point, Int)]

inRange :: Int -> (Int, Int) -> Bool
inRange x (lower, upper)
  | lower < upper = lower <= x && x <= upper
  | otherwise = upper <= x && x <= lower

intersects :: Segment -> Segment -> Bool
intersects VSegment {..} HSegment {..} = y `inRange` yRange && x `inRange` xRange
intersects HSegment {..} VSegment {..} = y `inRange` yRange && x `inRange` xRange
intersects _ _                         = False

intersections :: Segment -> Line -> Line
intersections segment = filter (segment `intersects`)

manhattanD :: Segment -> Segment -> Int
manhattanD VSegment {..} HSegment {..} = abs x + abs y
manhattanD HSegment {..} VSegment {..} = abs x + abs y
manhattanD _ _                         = 0

evaluateSteps :: Segment -> Segment -> Int
evaluateSteps VSegment {..} HSegment {..} = vSteps + hSteps - abs (snd xRange - x) - abs (snd yRange - y)
evaluateSteps HSegment {..} VSegment {..} = vSteps + hSteps - abs (snd xRange - x) - abs (snd yRange - y)
evaluateSteps _ _                         = 0

minCost :: (Line -> Segment -> [Int] -> [Int]) -> Line -> Line -> Int
minCost reducer line = minimum . filter (/= 0) . foldr (reducer line) []

distanceReducer :: Line -> Segment -> [Int] -> [Int]
distanceReducer line segment acc = (manhattanD segment <$> intersections segment line) ++ acc

stepsReducer :: Line -> Segment -> [Int] -> [Int]
stepsReducer line segment acc = (evaluateSteps segment <$> intersections segment line) ++ acc

inputParser :: ReadP [String]
inputParser = skipSpaces *> commaSeparatedSegments <* skipSpaces <* eof
  where
    commaSeparatedSegments = sepBy segment (char ',')
    directionLetter = choice [char 'U', char 'D', char 'L', char 'R']
    segment = directionLetter >>= \letter -> (letter :) <$> munch isDigit

parseLine :: [String] -> Line
parseLine = collectSegments (0, 0) [] 0

collectSegments :: Point -> StepsCache -> Int -> [String] -> Line
collectSegments _ _ _ [] = []
collectSegments point cache steps ([]:rest) = collectSegments point cache steps rest
collectSegments point cache stepCount input@((direction:range):_) =
  case lookup nPoint cache of
    Just s  -> collectSegmentsWith point nPoint cache s segType input
    Nothing -> collectSegmentsWith point nPoint ((nPoint, steps) : cache) steps segType input
  where
    distance = read range
    steps = stepCount + distance
    (nPoint, segType) = nextPoint direction point distance

collectSegmentsWith :: Point -> Point -> StepsCache -> Int -> SegmentType -> [String] -> Line
collectSegmentsWith _ _ _ _ _ [] = []
collectSegmentsWith (x0, y0) (x, y) cache steps segType (_:rest) =
  case segType of
    Vertical   -> VSegment x (y0, y) steps : collectSegments (x, y) cache steps rest
    Horizontal -> HSegment y (x0, x) steps : collectSegments (x, y) cache steps rest

nextPoint :: Char -> Point -> Int -> (Point, SegmentType)
nextPoint direction (x, y) distance =
  case direction of
    'U' -> ((x, y + distance), Vertical)
    'D' -> ((x, y - distance), Vertical)
    'L' -> ((x - distance, y), Horizontal)
    'R' -> ((x + distance, y), Horizontal)
    _   -> ((0, 0), Horizontal)

parseInput :: String -> Line
parseInput = parseLine . concatMap fst . readP_to_S inputParser

readLines :: IO [String]
readLines = filter (not . null) . lines <$> readFile "./resources/input-day3.txt"

solutionPart1 :: IO Int
solutionPart1 = do
  (line1:line2:_) <- fmap parseInput <$> readLines
  return $ minCost distanceReducer line1 line2

solutionPart2 :: IO Int
solutionPart2 = do
  (line1:line2:_) <- fmap parseInput <$> readLines
  return $ minCost stepsReducer line1 line2
