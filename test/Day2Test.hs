module Day2Test
  ( test
  ) where

import           Day2
import           UnitTest

realInput :: String
realInput = "./resources/input-day2.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 2
      , testCases =
          ( [ Assertion (Raw solutionPart1 "1,9,10,3,2,3,11,0,99,30,40,50") "3500"
            , Assertion (Raw solutionPart1 "1,0,0,0,99") "2"
            , Assertion (Raw solutionPart1 "1,1,1,4,99,5,6,0,99") "30"
            , Assertion (File solutionPart1 realInput) "3716293"
            ]
          , [Assertion (File solutionPart2 realInput) "6429"])
      }
