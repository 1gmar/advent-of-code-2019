module Day9
  ( solutionPart1
  , solutionPart2
  ) where

import           Util.IntCodeProgram

runBoostProgram :: [Int] -> [Int] -> ProgramResult
runBoostProgram inputData memory = runIntCodeProgram $ programWithInput (newProgram memory) inputData

solutionPart1 :: String -> Either String Int
solutionPart1 = fmap result . runBoostProgram [1] . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = fmap result . runBoostProgram [2] . parseIntCode
