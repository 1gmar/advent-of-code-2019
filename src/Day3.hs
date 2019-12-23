{-# LANGUAGE RecordWildCards #-}

module Day3
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (isControl, isDigit)
import           Text.ParserCombinators.ReadP (ReadP, char, choice, count, eof, munch, readP_to_S, satisfy, sepBy,
                                               skipSpaces)

data SegmentType
  = Vertical
  | Horizontal

data Segment
  = VSegment
      { x      :: Int
      , yRange :: Range
      , vSteps :: Int
      }
  | HSegment
      { y      :: Int
      , xRange :: Range
      , hSteps :: Int
      }

type Line = [Segment]

type Point = (Int, Int)

type Range = (Int, Int)

type StepsCache = [(Point, Int)]

type SegmentFolder = Segment -> Segment -> Int

type FoldState = (Line, (Point, StepsCache, Int))

inRange :: Int -> Range -> Bool
inRange x (lower, upper)
  | lower < upper = lower <= x && x <= upper
  | otherwise = upper <= x && x <= lower

intersects :: Segment -> Segment -> Bool
intersects HSegment {..} VSegment {..}       = y `inRange` yRange && x `inRange` xRange
intersects vs@VSegment {..} hs@HSegment {..} = intersects hs vs
intersects _ _                               = False

manhattanDistance :: Segment -> Segment -> Int
manhattanDistance HSegment {..} VSegment {..}       = abs x + abs y
manhattanDistance vs@VSegment {..} hs@HSegment {..} = manhattanDistance hs vs
manhattanDistance _ _                               = 0

stepDistance :: Segment -> Segment -> Int
stepDistance (HSegment y (_, x1) hSteps) (VSegment x (_, y1) vSteps) = vSteps + hSteps - abs (x1 - x) - abs (y1 - y)
stepDistance vs@VSegment {..} hs@HSegment {..}                       = stepDistance hs vs
stepDistance _ _                                                     = 0

minDistance :: SegmentFolder -> (Line, Line) -> Int
minDistance foldSeg (line1, line2) = (minimum . filter (/= 0) . foldr collect []) line2
  where
    collect segment acc = (foldSeg segment <$> filter (segment `intersects`) line1) ++ acc

parseLine :: [String] -> Line
parseLine = fst . foldl collectSegments ([], ((0, 0), [], 0))

collectSegments :: FoldState -> String -> FoldState
collectSegments (line, (point, cache, stepCount)) ~(direction:range) =
  case lookup nPoint cache of
    Just s  -> (nextSegment s : line, (nPoint, cache, s))
    Nothing -> (nextSegment steps : line, (nPoint, (nPoint, steps) : cache, steps))
  where
    distance = read range
    steps = stepCount + distance
    (nPoint, segType) = nextPoint direction point distance
    nextSegment = buildSegmentFor point nPoint segType

buildSegmentFor :: Point -> Point -> SegmentType -> Int -> Segment
buildSegmentFor (x0, y0) (x, y) segType steps =
  case segType of
    Vertical   -> VSegment x (y0, y) steps
    Horizontal -> HSegment y (x0, x) steps

nextPoint :: Char -> Point -> Int -> (Point, SegmentType)
nextPoint direction (x, y) distance =
  case direction of
    'U' -> ((x, y + distance), Vertical)
    'D' -> ((x, y - distance), Vertical)
    'L' -> ((x - distance, y), Horizontal)
    _   -> ((x + distance, y), Horizontal)

inputParser :: ReadP [Line]
inputParser = skipSpaces *> count 2 (line <* endOfLine) <* skipSpaces <* eof
  where
    endOfLine = satisfy isControl
    line = parseLine <$> segment `sepBy` char ','
    segment = directionLetter >>= \letter -> (letter :) <$> munch isDigit
    directionLetter = choice [char 'U', char 'D', char 'L', char 'R']

parseInput :: String -> [Line]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [Line]
readInput = parseInput <$> readFile "./resources/input-day3.txt"

linesToTuple :: [Line] -> Maybe (Line, Line)
linesToTuple input =
  case input of
    [line1, line2] -> Just (line1, line2)
    _              -> Nothing

solutionPart1 :: IO (Maybe Int)
solutionPart1 = fmap (minDistance manhattanDistance) . linesToTuple <$> readInput

solutionPart2 :: IO (Maybe Int)
solutionPart2 = fmap (minDistance stepDistance) . linesToTuple <$> readInput
