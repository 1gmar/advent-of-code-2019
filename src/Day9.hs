module Day9
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

runBoostProgram :: [Int] -> [Int] -> ProgramResult
runBoostProgram inputData prog = runIntCodeProgram $ programWithInput prog inputData

solutionPart1 :: String -> String
solutionPart1 = showResult . runBoostProgram [1] . parseIntCode

solutionPart2 :: String -> String
solutionPart2 = showResult . runBoostProgram [2] . parseIntCode
