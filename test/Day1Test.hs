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
    [ AssertData Raw solutionPart1 "12" 2
    , AssertData Raw solutionPart1 "14" 2
    , AssertData Raw solutionPart1 "1969" 654
    , AssertData Raw solutionPart1 "100756" 33583
    , AssertData File solutionPart1 realInput 3423511
    ]
    [ AssertData Raw solutionPart2 "14" 2
    , AssertData Raw solutionPart2 "1969" 966
    , AssertData Raw solutionPart2 "100756" 50346
    , AssertData File solutionPart2 realInput 5132379
    ]
