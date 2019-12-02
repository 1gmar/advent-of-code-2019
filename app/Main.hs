module Main where

import           Day1
import           Day2

main :: IO ()
main = do
  solutionDay1Part1 >>= print
  solutionDay1Part2 >>= print
  solutionDay2Part1 >>= print
  solutionDay2Part2 >>= print
