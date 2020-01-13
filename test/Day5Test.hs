module Day5Test
  ( test
  ) where

import           Day5
import           UnitTest

realInput :: String
realInput = "./resources/input-day5.txt"

testCase :: String
testCase =
  "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
  \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
  \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

test :: IO ()
test =
  runTest
    DayTest
      { day = 5
      , testCases =
          ( [ Assertion (Constant solutionPart1 "3,9,8,9,10,9,4,9,99,-1,8") "0"
            , Assertion (Constant solutionPart1 "3,9,7,9,10,9,4,9,99,-1,8") "1"
            , Assertion (Constant solutionPart1 "3,3,1108,-1,8,3,4,3,99") "0"
            , Assertion (Constant solutionPart1 "3,3,1107,-1,8,3,4,3,99") "1"
            , Assertion (File solutionPart1 realInput) "15386262"
            ]
          , [ Assertion (Constant solutionPart2 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") "1"
            , Assertion (Constant solutionPart2 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") "1"
            , Assertion (Constant solutionPart2 testCase) "999"
            , Assertion (File solutionPart2 realInput) "10376124"
            ])
      }
