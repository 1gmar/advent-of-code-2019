module Day15Test
  ( test,
  )
where

import Day15
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day15.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 15,
        part1 = (solutionPart1, [fileData realInput `ShouldBe` Const (Right 272)]),
        part2 = (solutionPart2, [fileData realInput `ShouldBe` Const (Right 398)])
      }
