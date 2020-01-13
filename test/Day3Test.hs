module Day3Test
  ( test
  ) where

import           Day3
import           UnitTest

realInput :: String
realInput = "./resources/input-day3.txt"

testCase1 :: String
testCase1 = unlines ["R8,U5,L5,D3", "U7,R6,D4,L4"]

testCase2 :: String
testCase2 = unlines ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]

testCase3 :: String
testCase3 = unlines ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

test :: IO ()
test =
  runTest
    DayTest
      { day = 3
      , testCases =
          ( [ Assertion (Constant solutionPart1 testCase1) (Just 6)
            , Assertion (Constant solutionPart1 testCase2) (Just 159)
            , Assertion (Constant solutionPart1 testCase3) (Just 135)
            , Assertion (File solutionPart1 realInput) (Just 293)
            ]
          , [ Assertion (Constant solutionPart2 testCase1) (Just 30)
            , Assertion (Constant solutionPart2 testCase2) (Just 610)
            , Assertion (Constant solutionPart2 testCase3) (Just 410)
            , Assertion (File solutionPart2 realInput) (Just 27306)
            ])
      }
