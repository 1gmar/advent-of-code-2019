module Day3Test
  ( test,
  )
where

import Day3
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day3.txt"

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
      { day = 3,
        part1 =
          ( solutionPart1,
            [ Const testCase1 `ShouldBe` Const (Just 6),
              Const testCase2 `ShouldBe` Const (Just 159),
              Const testCase3 `ShouldBe` Const (Just 135),
              fileData realInput `ShouldBe` Const (Just 293)
            ]
          ),
        part2 =
          ( solutionPart2,
            [ Const testCase1 `ShouldBe` Const (Just 30),
              Const testCase2 `ShouldBe` Const (Just 610),
              Const testCase3 `ShouldBe` Const (Just 410),
              fileData realInput `ShouldBe` Const (Just 27306)
            ]
          )
      }
