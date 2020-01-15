module Day1Test
  ( test
  ) where

import           Day1
import           UnitTest

realInput :: String
realInput = "./resources/input-day1.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 1
      , testCases =
          ( [ Assertion (Constant "12") solutionPart1 2
            , Assertion (Constant "14") solutionPart1 2
            , Assertion (Constant "1969") solutionPart1 654
            , Assertion (Constant "100756") solutionPart1 33583
            , Assertion (fileInput realInput) solutionPart1 3423511
            ]
          , [ Assertion (Constant "12") solutionPart2 2
            , Assertion (Constant "14") solutionPart2 2
            , Assertion (Constant "1969") solutionPart2 966
            , Assertion (Constant "100756") solutionPart2 50346
            , Assertion (fileInput realInput) solutionPart2 5132379
            ])
      }
