module Day9
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

runBoostProgram :: [Int] -> [String] -> ProgramResult
runBoostProgram _ []           = Left "Program is empty."
runBoostProgram inputData prog = runIntCodeProgram $ ProgramState 0 inputData prog 0 0 False False

inputFile :: String
inputFile = "./resources/input-day9.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showResult . runBoostProgram [1]

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= putStrLn . showResult . runBoostProgram [2]
