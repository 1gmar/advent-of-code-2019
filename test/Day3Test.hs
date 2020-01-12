module Day3Test
  ( runTests
  ) where

import           AssertUtils
import           Day3

realInput :: String
realInput = "./resources/input-day3.txt"

testCase1 :: [String]
testCase1 = ["R8,U5,L5,D3", "U7,R6,D4,L4"]

testCase2 :: [String]
testCase2 = ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]

testCase3 :: [String]
testCase3 = ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

runTests :: IO ()
runTests =
  runAssertions
    3
    [ AssertData Raw solutionPart1 (unlines testCase1) (Just 6)
    , AssertData Raw solutionPart1 (unlines testCase2) (Just 159)
    , AssertData Raw solutionPart1 (unlines testCase3) (Just 135)
    , AssertData File solutionPart1 realInput (Just 293)
    ]
    [ AssertData Raw solutionPart2 (unlines testCase1) (Just 30)
    , AssertData Raw solutionPart2 (unlines testCase2) (Just 610)
    , AssertData Raw solutionPart2 (unlines testCase3) (Just 410)
    , AssertData File solutionPart2 realInput (Just 27306)
    ]
