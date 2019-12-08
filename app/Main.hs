module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

main :: IO ()
main = do
  putStrLn "Day1:"
  Day1.solutionPart1 >>= print
  Day1.solutionPart2 >>= print
  putStrLn "Day2:"
  Day2.solutionPart1 >>= print
  Day2.solutionPart2 >>= print
  putStrLn "Day3:"
  Day3.solutionPart1 >>= print
  Day3.solutionPart2 >>= print
  putStrLn "Day4:"
  print Day4.solutionPart1
  print Day4.solutionPart2
  putStrLn "Day 5:"
  Day5.solutionPart1
  Day5.solutionPart2
