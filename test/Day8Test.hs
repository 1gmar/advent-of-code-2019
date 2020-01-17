module Day8Test
  ( test
  ) where

import           Day8
import           UnitTest

realInput :: String
realInput = "./resources/input-day8.txt"

realOutput :: String
realOutput = "./out/output-day8.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 8
      , testCases =
          ( [Assertion (fileSource realInput) solutionPart1 (Constant 2413)]
          , [Assertion (fileSource realInput) solutionPart2 (fileSource realOutput)])
      }
