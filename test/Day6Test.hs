module Day6Test
  ( test
  ) where

import           Day6
import           UnitTest

realInput :: String
realInput = "./resources/input/day6.txt"

testCase1 :: String
testCase1 = unlines ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

testCase2 :: String
testCase2 = unlines ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

test :: IO ()
test =
  runTest
    DayTest
      { day = 6
      , part1 =
          ( solutionPart1
          , [Assertion (Constant testCase1) (Constant 42), Assertion (fileSource realInput) (Constant 314247)])
      , part2 =
          ( solutionPart2
          , [Assertion (Constant testCase2) (Constant 4), Assertion (fileSource realInput) (Constant 514)])
      }
