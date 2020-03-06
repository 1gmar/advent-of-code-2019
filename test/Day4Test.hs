module Day4Test
  ( test
  ) where

import           Day4
import           Util.UnitTest

test :: IO ()
test =
  runTest
    DayTest
      { day = 4
      , part1 = (solutionPart1, [Assertion (Constant (134792, 675810)) (Constant 1955)])
      , part2 = (solutionPart2, [Assertion (Constant (134792, 675810)) (Constant 1319)])
      }
