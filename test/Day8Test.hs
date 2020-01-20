module Day8Test
  ( test
  ) where

import           Day8
import           UnitTest

realInput :: String
realInput = "./resources/input/day8.txt"

realOutput :: String
realOutput = "./resources/output/day8.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 8
      , part1 = (solutionPart1, [Assertion (fileSource realInput) (Constant 2413)])
      , part2 = (solutionPart2, [Assertion (fileSource realInput) (fileSource realOutput)])
      }
