module Day9
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

runDiagnosticProgram :: [Int] -> [String] -> ProgramOutput
runDiagnosticProgram _ []           = Left "Program is empty."
runDiagnosticProgram inputData prog = runIntCodeProgram $ ProgramState 0 inputData prog 0 0 False False

showProgramOutput :: ProgramOutput -> String
showProgramOutput (Left err)    = "Error: " ++ err
showProgramOutput (Right state) = show $ result state

inputFile :: String
inputFile = "./resources/input-day9.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showProgramOutput . runDiagnosticProgram [1]

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= putStrLn . showProgramOutput . runDiagnosticProgram [2]
