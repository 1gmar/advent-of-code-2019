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
          ( [Assertion (Constant solutionPart1 (134792, 675810)) 1955]
          , [Assertion (Constant solutionPart2 (134792, 675810)) 1319])
      }
