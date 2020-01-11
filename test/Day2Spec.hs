module Day2Spec
  ( runTests
  ) where

import           AssertUtils
import           Day2

realInput :: String
realInput = "./resources/input-day2.txt"

runTests :: IO ()
runTests =
  runAssertions
    2
    [ AssertData Raw solutionPart1 "1,9,10,3,2,3,11,0,99,30,40,50" "3500"
    , AssertData Raw solutionPart1 "1,0,0,0,99" "2"
    , AssertData Raw solutionPart1 "1,1,1,4,99,5,6,0,99" "30"
    , AssertData File solutionPart1 realInput "3716293"
    ]
    [AssertData File solutionPart2 realInput "6429"]
