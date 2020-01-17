{-# LANGUAGE RecordWildCards #-}

module Day3
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Maybe (mapMaybe)
import           ParseUtils

data SegmentType
  = Vertical
  | Horizontal

data Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT

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

type DistanceFun = Segment -> Segment -> Maybe Int

type FoldState = (Line, (Point, StepsCache, Int))

toDirection :: Char -> Direction
toDirection dirChar =
  case dirChar of
    'U' -> UP
    'D' -> DOWN
    'L' -> LEFT
    _   -> RIGHT

inRange :: Int -> Range -> Bool
inRange x (lower, upper)
  | lower < upper = lower <= x && x <= upper
  | otherwise = upper <= x && x <= lower

intersects :: Segment -> Segment -> Bool
intersects HSegment {..} VSegment {..}       = y `inRange` yRange && x `inRange` xRange
intersects vs@VSegment {..} hs@HSegment {..} = intersects hs vs
intersects _ _                               = False

manhattanDistance :: Segment -> Segment -> Maybe Int
manhattanDistance HSegment {..} VSegment {..}       = Just $ abs x + abs y
manhattanDistance vs@VSegment {..} hs@HSegment {..} = manhattanDistance hs vs
manhattanDistance _ _                               = Nothing

stepDistance :: Segment -> Segment -> Maybe Int
stepDistance (HSegment y (_, x1) s1) (VSegment x (_, y1) s2) = Just $ s1 + s2 - abs (x1 - x) - abs (y1 - y)
stepDistance vs@VSegment {..} hs@HSegment {..}               = stepDistance hs vs
stepDistance _ _                                             = Nothing

minDistance :: DistanceFun -> (Line, Line) -> Int
minDistance distFun (line1, line2) = (minimum . filter (/= 0) . foldr collect []) line2
  where
    collect segment acc = mapMaybe (distFun segment) (intersectsLine segment) ++ acc
    intersectsLine segment = filter (segment `intersects`) line1

parseLine :: [(Direction, Int)] -> Line
parseLine = fst . foldl collectSegments ([], ((0, 0), [], 0))

collectSegments :: FoldState -> (Direction, Int) -> FoldState
collectSegments (line, (point, cache, stepCount)) (direction, range) =
  case lookup nPoint cache of
    Just s  -> (nextSegment s : line, (nPoint, cache, s))
    Nothing -> (nextSegment steps : line, (nPoint, (nPoint, steps) : cache, steps))
  where
    steps = stepCount + range
    (nPoint, segType) = nextPoint direction point range
    nextSegment = buildSegmentFor point nPoint segType

buildSegmentFor :: Point -> Point -> SegmentType -> Int -> Segment
buildSegmentFor (x0, y0) (x, y) segType steps =
  case segType of
    Vertical   -> VSegment x (y0, y) steps
    Horizontal -> HSegment y (x0, x) steps

nextPoint :: Direction -> Point -> Int -> (Point, SegmentType)
nextPoint direction (x, y) distance =
  case direction of
    UP    -> ((x, y + distance), Vertical)
    DOWN  -> ((x, y - distance), Vertical)
    LEFT  -> ((x - distance, y), Horizontal)
    RIGHT -> ((x + distance, y), Horizontal)

inputParser :: ReadP [Line]
inputParser = trimSpacesEOF $ count 2 (line <* endOfLine)
  where
    line = parseLine <$> segment `sepBy` char ','
    segment = (,) <$> direction <*> integer
    direction = toDirection <$> choice [char 'U', char 'D', char 'L', char 'R']

maybeTwoLines :: [Line] -> Maybe (Line, Line)
maybeTwoLines input =
  case input of
    [line1, line2] -> Just (line1, line2)
    _              -> Nothing

solutionPart1 :: String -> Maybe Int
solutionPart1 = fmap (minDistance manhattanDistance) . maybeTwoLines . parseInput inputParser

solutionPart2 :: String -> Maybe Int
solutionPart2 = fmap (minDistance stepDistance) . maybeTwoLines . parseInput inputParser
