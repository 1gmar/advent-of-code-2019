module Main where

import qualified Day1
import qualified Day2
import qualified Day3

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
