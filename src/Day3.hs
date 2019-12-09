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

minDistance :: (Segment -> Segment -> Int) -> Line -> Line -> Int
minDistance combineSeg line = minimum . filter (/= 0) . foldr collect []
  where
    collect segment acc = (combineSeg segment <$> filter (segment `intersects`) line) ++ acc

parseLine :: [String] -> Line
parseLine = collectSegments (0, 0) [] 0

collectSegments :: Point -> StepsCache -> Int -> [String] -> Line
collectSegments _ _ _ [] = []
collectSegments point cache steps ([]:rest) = collectSegments point cache steps rest
collectSegments point cache stepCount input@((direction:range):_) =
  case lookup nPoint cache of
    Just s  -> buildSegmentFor point nPoint cache s segType input
    Nothing -> buildSegmentFor point nPoint ((nPoint, steps) : cache) steps segType input
  where
    distance = read range
    steps = stepCount + distance
    (nPoint, segType) = nextPoint direction point distance

buildSegmentFor :: Point -> Point -> StepsCache -> Int -> SegmentType -> [String] -> Line
buildSegmentFor _ _ _ _ _ [] = []
buildSegmentFor (x0, y0) (x, y) cache steps segType (_:rest) =
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

inputParser :: ReadP [String]
inputParser = skipSpaces *> commaSeparatedSegments <* skipSpaces <* eof
  where
    commaSeparatedSegments = sepBy segment (char ',')
    segment = directionLetter >>= \letter -> (letter :) <$> munch isDigit
    directionLetter = choice [char 'U', char 'D', char 'L', char 'R']

parseInput :: String -> Line
parseInput = parseLine . concatMap fst . readP_to_S inputParser

readLines :: IO [String]
readLines = filter (not . null) . lines <$> readFile "./resources/input-day3.txt"

solutionPart1 :: IO Int
solutionPart1 = do
  (line1:line2:_) <- fmap parseInput <$> readLines
  return $ minDistance manhattanDistance line1 line2

solutionPart2 :: IO Int
solutionPart2 = do
  (line1:line2:_) <- fmap parseInput <$> readLines
  return $ minDistance stepDistance line1 line2
