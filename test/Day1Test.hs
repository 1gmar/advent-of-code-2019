module Day1Test
  ( runTests
  ) where

import           AssertUtils
import           Day1

realInput :: String
realInput = "./resources/input-day1.txt"

runTests :: IO ()
runTests =
  runAssertions
    1
    [ Assertion (Raw solutionPart1 "12") 2
    , Assertion (Raw solutionPart1 "14") 2
    , Assertion (Raw solutionPart1 "1969") 654
    , Assertion (Raw solutionPart1 "100756") 33583
    , Assertion (File solutionPart1 realInput) 3423511
    ]
    [ Assertion (Raw solutionPart2 "14") 2
    , Assertion (Raw solutionPart2 "1969") 966
    , Assertion (Raw solutionPart2 "100756") 50346
    , Assertion (File solutionPart2 realInput) 5132379
    ]
