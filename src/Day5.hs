module Day5
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

runDiagnosticProgram :: [Int] -> [Int] -> ProgramResult
runDiagnosticProgram inputData prog = runIntCodeProgram $ programWithInput prog inputData

solutionPart1 :: String -> String
solutionPart1 = showResult . runDiagnosticProgram [1] . parseInput

solutionPart2 :: String -> String
solutionPart2 = showResult . runDiagnosticProgram [5] . parseInput
