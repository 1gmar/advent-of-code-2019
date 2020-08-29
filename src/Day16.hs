{-# LANGUAGE TupleSections #-}

module Day16
  ( solutionPart1
  , solutionPart2
  ) where

import           Control.Monad       (forM_, replicateM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Char           (digitToInt, isDigit)
import           Data.STRef          (modifySTRef, newSTRef, readSTRef)
import           Data.Vector         (Vector, enumFromN, fromList, iterateN, slice, unsafeFreeze, unsafeThaw, (!))
import qualified Data.Vector         as V (foldl, foldr, last, length, take)
import           Data.Vector.Mutable (STVector, write)
import           Data.Vector.Mutable as VM (length, read)
import           Util.ParseUtils

type IntVector = Vector Int

type CellVector = Vector (Int, Int)

type PatternMatrix = Vector CellVector

sparseRow :: Int -> Int -> Int -> [Int]
sparseRow size row start = concat [intervals x | x <- take row [start ..]]
  where
    intervals pos = takeWhile (< size) [pos - 1, pos - 1 + row * 4 ..]

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

offsetPhase :: STVector s Int -> Int -> ST s ()
offsetPhase stVector offset = do
  let end = VM.length stVector - 1
  sumRef <- newSTRef 0
  forM_ [end,end - 1 .. offset] $ \i -> do
    digit <- VM.read stVector i
    modifySTRef sumRef (sumDigits digit)
    readSTRef sumRef >>= write stVector i
  where
    sumDigits digit s = (s + digit) `mod` 10

findEmbeddedMsg :: IntVector -> Int
findEmbeddedMsg digits =
  runST $ do
    let offset = digitsToInt $ V.take 7 digits
    let realSignal = mconcat $ replicate 10000 digits
    stVector <- unsafeThaw realSignal
    replicateM_ 100 $ offsetPhase stVector offset
    vector <- unsafeFreeze stVector
    return $ readMessage offset vector
  where
    readMessage offset = digitsToInt . slice offset 8

inputParser :: ReadP [Int]
inputParser = trimSpacesEOF $ many1 digit
  where
    digit = digitToInt <$> satisfy isDigit

solutionPart1 :: String -> Int
solutionPart1 = digitsToInt . V.take 8 . runFFT 100 . fromList . parseInput inputParser

solutionPart2 :: String -> Int
solutionPart2 = findEmbeddedMsg . fromList . parseInput inputParser
