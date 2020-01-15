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
          ( [ Assertion (Constant testCase1) solutionPart1 (Just 6)
            , Assertion (Constant testCase2) solutionPart1 (Just 159)
            , Assertion (Constant testCase3) solutionPart1 (Just 135)
            , Assertion (fileInput realInput) solutionPart1 (Just 293)
            ]
          , [ Assertion (Constant testCase1) solutionPart2 (Just 30)
            , Assertion (Constant testCase2) solutionPart2 (Just 610)
            , Assertion (Constant testCase3) solutionPart2 (Just 410)
            , Assertion (fileInput realInput) solutionPart2 (Just 27306)
            ])
      }
