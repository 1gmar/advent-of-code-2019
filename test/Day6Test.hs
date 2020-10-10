module Day6Test
  ( test,
  )
where

import Day6
import Util.UnitTest

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
      { day = 6,
        part1 =
          ( solutionPart1,
            [Const testCase1 `ShouldBe` Const 42, fileData realInput `ShouldBe` Const 314247]
          ),
        part2 =
          ( solutionPart2,
            [Const testCase2 `ShouldBe` Const 4, fileData realInput `ShouldBe` Const 514]
          )
      }
