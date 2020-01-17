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
          ( [Assertion (Constant (134792, 675810)) solutionPart1 (Constant 1955)]
          , [Assertion (Constant (134792, 675810)) solutionPart2 (Constant 1319)])
      }
