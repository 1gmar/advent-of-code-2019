module Day6Test
  ( test
  ) where

import           Day6
import           UnitTest

realInput :: String
realInput = "./resources/input-day6.txt"

testCase1 :: String
testCase1 = unlines ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

testCase2 :: String
testCase2 = unlines ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

test :: IO ()
test =
  runTest
    DayTest
      { day = 6
      , testCases =
          ( [Assertion (Raw solutionPart1 testCase1) 42, Assertion (File solutionPart1 realInput) 314247]
          , [Assertion (Raw solutionPart2 testCase2) 4, Assertion (File solutionPart2 realInput) 514])
      }
