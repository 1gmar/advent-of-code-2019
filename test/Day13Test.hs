module Day13Test
  ( test
  ) where

import           Day13
import           Util.UnitTest

realInput :: String
realInput = "./resources/input/day13.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 13
      , part1 = (solutionPart1, [Assertion (fileSource realInput) (Constant $ Right 298)])
      , part2 = (solutionPart2, [Assertion (fileSource realInput) (Constant $ Right 13956)])
      }
