module Day2
  ( solutionDay2Part1
  , solutionDay2Part2
  ) where

import           Data.Char                    (isDigit)
import           Data.Foldable                (foldr')
import           Data.Maybe                   (listToMaybe)
import           Text.ParserCombinators.ReadP (ReadP, char, eof, munch, readP_to_S, sepBy, skipSpaces)

append :: [Int] -> [Int] -> [Int]
append = flip (foldr' (:))

replaceAt :: Int -> [Int] -> [Int] -> [Int]
replaceAt pos values list =
  let (upper, lower) = splitAt pos list
      suffix = drop (length values) lower
   in upper `append` (values `append` suffix)

elemAt :: [Int] -> Int -> Maybe Int
elemAt list = listToMaybe . flip drop list

runIntCodeProgram :: [Int] -> Maybe [Int]
runIntCodeProgram []   = Nothing
runIntCodeProgram list = runIndexedIntCodeProgram 0 list

runIndexedIntCodeProgram :: Int -> [Int] -> Maybe [Int]
runIndexedIntCodeProgram index list
  | 99 `elem` list `elemAt` index = Just list
  | otherwise = processIntCode $ drop index list
  where
    processIntCode []                               = Nothing
    processIntCode (1:leftIdx:rightIdx:targetIdx:_) = computeValueAt targetIdx leftIdx (+) rightIdx
    processIntCode (2:leftIdx:rightIdx:targetIdx:_) = computeValueAt targetIdx leftIdx (*) rightIdx
    computeValueAt targetIdx leftIdx op rightIdx = do
      newValue <- op <$> list `elemAt` leftIdx <*> list `elemAt` rightIdx
      runIndexedIntCodeProgram (index + 4) $ replaceAt targetIdx [newValue] list

findInputPairFor :: Int -> [Int] -> Maybe (Int, Int)
findInputPairFor result input =
  listToMaybe [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99], result `elem` computeOutputFor noun verb]
  where
    computeOutputFor noun verb = runIntCodeProgram (inputSampleFor noun verb) >>= listToMaybe
    inputSampleFor noun verb = replaceAt 1 [noun, verb] input

computeNounVerbChecksum :: (Int, Int) -> Int
computeNounVerbChecksum (noun, verb) = 100 * noun + verb

inputParser :: ReadP [Int]
inputParser = map read <$> commaSeparatedDigits <* eof
  where
    commaSeparatedDigits = skipSpaces *> sepBy (munch isDigit) (char ',') <* skipSpaces

parseInput :: String -> [Int]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [Int]
readInput = parseInput <$> readFile "./resources/input-day2.txt"

solutionDay2Part1 :: IO (Maybe Int)
solutionDay2Part1 = do
  list <- runIntCodeProgram <$> readInput
  return $ list >>= listToMaybe

solutionDay2Part2 :: IO (Maybe Int)
solutionDay2Part2 = do
  inputPair <- findInputPairFor 19690720 <$> readInput
  return $ computeNounVerbChecksum <$> inputPair
