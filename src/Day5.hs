module Day5
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

runDiagnosticProgram :: [Int] -> [String] -> ProgramResult
runDiagnosticProgram inputData prog = runIntCodeProgram $ programWithInput prog inputData

inputFile :: String
inputFile = "./resources/input-day5.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showResult . runDiagnosticProgram [1]

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= putStrLn . showResult . runDiagnosticProgram [5]
