module Day17Test
  ( test,
  )
where

import Day17
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day17.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 17,
        part1 = (solutionPart1, [fileData realInput `ShouldBe` Const (Right 7404)]),
        part2 = (solutionPart2, [fileData realInput `ShouldBe` Const (Right 929045)])
      }
