{-# LANGUAGE TupleSections #-}

module Day16
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char   (digitToInt, isDigit)
import           Data.Vector (Vector, cons, empty, enumFromN, fromList, iterateN, (!))
import qualified Data.Vector as V (drop, foldl, foldr, last, length, take)
import           ParseUtils

type IntVector = Vector Int

type CellVector = Vector (Int, Int)

type PatternMatrix = Vector CellVector

sparseRow :: Int -> Int -> Int -> [Int]
sparseRow size row start = concat [intervals x | x <- take row [start ..]]
  where
    intervals pos = takeWhile (< size) [pos - 1,pos - 1 + row * 4 ..]

phasePattern :: Int -> Int -> CellVector
phasePattern size pos = fromList $ ones ++ negativeOnes
  where
    ones = (, 1) <$> sparseRow size pos pos
    negativeOnes = (, -1) <$> sparseRow size pos (pos + 2 * pos)

patternMatrix :: Int -> PatternMatrix
patternMatrix size = phasePattern size <$> enumFromN 1 size

nextDigit :: IntVector -> CellVector -> Int
nextDigit digits = (`mod` 10) . abs . V.foldl sumDigits 0
  where
    sumDigits total (position, value) = total + (digits ! position * value)

nextPhaseDigits :: IntVector -> PatternMatrix -> IntVector
nextPhaseDigits digits = fmap (nextDigit digits)

runFFT :: Int -> IntVector -> IntVector
runFFT phases digits = (V.last . iterateN phases nextPhase . nextPhase) digits
  where
    pMatrix = patternMatrix $ V.length digits
    nextPhase = (`nextPhaseDigits` pMatrix)

digitsToInt :: IntVector -> Int
digitsToInt = fst . V.foldr toInt (0, 0)
  where
    toInt :: Int -> (Int, Int) -> (Int, Int)
    toInt digit (int, pos) = (digit * (10 ^ pos) + int, pos + 1)

offsetPhase :: IntVector -> IntVector
offsetPhase = fst . V.foldr sumDigits (empty, 0)
  where
    sumDigits digit (vector, s) =
      let nextD = (s + digit) `mod` 10
       in (nextD `cons` vector, nextD)

findEmbeddedMsg :: IntVector -> Int
findEmbeddedMsg digits = readMessage $ realSignal digits
  where
    offset = digitsToInt $ V.take 7 digits
    realSignal = V.drop offset . mconcat . replicate 10000
    finalDigits = V.last . iterateN 100 offsetPhase . offsetPhase
    readMessage = digitsToInt . V.take 8 . finalDigits

inputParser :: ReadP [Int]
inputParser = trimSpacesEOF $ many1 digit
  where
    digit = digitToInt <$> satisfy isDigit

solutionPart1 :: String -> Int
solutionPart1 = digitsToInt . V.take 8 . runFFT 100 . fromList . parseInput inputParser

solutionPart2 :: String -> Int
solutionPart2 = findEmbeddedMsg . fromList . parseInput inputParser
