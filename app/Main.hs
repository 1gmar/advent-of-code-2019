module Main where

import qualified Day1
import qualified Day2

main :: IO ()
main = do
  putStrLn "Day1:"
  Day1.solutionPart1 >>= print
  Day1.solutionPart2 >>= print
  putStrLn "Day2:"
  Day2.solutionPart1 >>= print
  Day2.solutionPart2 >>= print
