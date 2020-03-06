module Day15Test
  ( test
  ) where

import           Day15
import           Util.UnitTest

realInput :: String
realInput = "./resources/input/day15.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 15
      , part1 = (solutionPart1, [Assertion (fileSource realInput) (Constant $ Right 272)])
      , part2 = (solutionPart2, [Assertion (fileSource realInput) (Constant $ Right 398)])
      }
