module Day2Test
  ( test
  ) where

import           Day2
import           UnitTest

realInput :: String
realInput = "./resources/input/day2.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 2
      , testCases =
          ( [ Assertion (Constant "1,9,10,3,2,3,11,0,99,30,40,50") solutionPart1 (Constant "3500")
            , Assertion (Constant "1,0,0,0,99") solutionPart1 (Constant "2")
            , Assertion (Constant "1,1,1,4,99,5,6,0,99") solutionPart1 (Constant "30")
            , Assertion (fileSource realInput) solutionPart1 (Constant "3716293")
            ]
          , [Assertion (fileSource realInput) solutionPart2 (Constant "6429")])
      }
