module Day4Test
  ( test
  ) where

import           Day4
import           UnitTest

test :: IO ()
test =
  runTest
    DayTest
      { day = 4
      , testCases =
          ([Assertion (Raw solutionPart1 (134792, 675810)) 1955], [Assertion (Raw solutionPart2 (134792, 675810)) 1319])
      }
