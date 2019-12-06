module Day2
  ( solutionPart1
  , solutionPart2
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

runIntCodeProgram :: [Int] -> Maybe Int
runIntCodeProgram []   = Nothing
runIntCodeProgram list = runIndexedIntCodeProgram 0 list

runIndexedIntCodeProgram :: Int -> [Int] -> Maybe Int
runIndexedIntCodeProgram index list
  | 99 `elem` list `elemAt` index = listToMaybe list
  | otherwise = processIntCode $ drop index list
  where
    processIntCode []                                = Nothing
    processIntCode (op:leftIdx:rightIdx:targetIdx:_) = operator op >>= computeValueAt targetIdx leftIdx rightIdx
    computeValueAt targetIdx leftIdx rightIdx op = do
      newValue <- op <$> list `elemAt` leftIdx <*> list `elemAt` rightIdx
      runIndexedIntCodeProgram (index + 4) $ replaceAt targetIdx [newValue] list

operator :: Int -> Maybe (Int -> Int -> Int)
operator 1 = Just (+)
operator 2 = Just (*)
operator _ = Nothing

findInputPairFor :: Int -> [Int] -> Maybe (Int, Int)
findInputPairFor result input =
  listToMaybe [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99], result `elem` computeOutputFor noun verb]
  where
    computeOutputFor noun verb = runIntCodeProgram $ replaceAt 1 [noun, verb] input

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

solutionPart1 :: IO (Maybe Int)
solutionPart1 = runIntCodeProgram <$> readInput

solutionPart2 :: IO (Maybe Int)
solutionPart2 = fmap computeNounVerbChecksum . findInputPairFor 19690720 <$> readInput
