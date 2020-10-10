module Day16Test
  ( test,
  )
where

import Day16
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day16.txt"

testCase1 :: String
testCase1 = "80871224585914546619083218645595"

testCase2 :: String
testCase2 = "19617804207202209144916044189917"

testCase3 :: String
testCase3 = "69317163492948606335995924319873"

testCase4 :: String
testCase4 = "03036732577212944063491565474664"

testCase5 :: String
testCase5 = "02935109699940807407585447034323"

testCase6 :: String
testCase6 = "03081770884921959731165446850517"

test :: IO ()
test =
  runTest
    DayTest
      { day = 16,
        part1 =
          ( solutionPart1,
            [ Const testCase1 `ShouldBe` Const 24176176,
              Const testCase2 `ShouldBe` Const 73745418,
              Const testCase3 `ShouldBe` Const 52432133,
              fileData realInput `ShouldBe` Const 84487724
            ]
          ),
        part2 =
          ( solutionPart2,
            [ Const testCase4 `ShouldBe` Const 84462026,
              Const testCase5 `ShouldBe` Const 78725270,
              Const testCase6 `ShouldBe` Const 53553731,
              fileData realInput `ShouldBe` Const 84692524
            ]
          )
      }
