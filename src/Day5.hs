module Day5
  ( solutionPart1
  , solutionPart2
  ) where

import           Util.IntCodeProgram

runDiagnosticProgram :: [Int] -> [Int] -> ProgramResult
runDiagnosticProgram inputData prog = runIntCodeProgram $ programWithInput prog inputData

solutionPart1 :: String -> Either String Int
solutionPart1 = fmap result . runDiagnosticProgram [1] . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = fmap result . runDiagnosticProgram [5] . parseIntCode
