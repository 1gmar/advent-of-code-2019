module Day1Test
  ( test,
  )
where

import Day1
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day1.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 1,
        part1 =
          ( solutionPart1,
            [ Const "12" `ShouldBe` Const 2,
              Const "14" `ShouldBe` Const 2,
              Const "1969" `ShouldBe` Const 654,
              Const "100756" `ShouldBe` Const 33583,
              fileData realInput `ShouldBe` Const 3423511
            ]
          ),
        part2 =
          ( solutionPart2,
            [ Const "12" `ShouldBe` Const 2,
              Const "14" `ShouldBe` Const 2,
              Const "1969" `ShouldBe` Const 966,
              Const "100756" `ShouldBe` Const 50346,
              fileData realInput `ShouldBe` Const 5132379
            ]
          )
      }
