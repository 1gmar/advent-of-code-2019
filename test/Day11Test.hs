module Day11Test
  ( test
  ) where

import           Day11
import           Util.UnitTest

realInput :: String
realInput = "./resources/input/day11.txt"

realOutput :: String
realOutput = "./resources/output/day11.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 11
      , part1 = (solutionPart1, [Assertion (fileSource realInput) (Constant $ Right 2016)])
      , part2 = (solutionPart2, [Assertion (fileSource realInput) (fileSourceM Right realOutput)])
      }
