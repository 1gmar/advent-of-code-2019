module Main where

import qualified Day1
import qualified Day2

main :: IO ()
main = do
  Day1.solutionPart1 >>= print
  Day1.solutionPart2 >>= print
  Day2.solutionPart1 >>= print
  Day2.solutionPart2 >>= print
