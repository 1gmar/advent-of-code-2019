module Day11Test
  ( test,
  )
where

import Day11
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day11.txt"

realOutput :: String
realOutput = "./resources/output/day11.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 11,
        part1 = (solutionPart1, [fileData realInput `ShouldBe` Const (Right 2016)]),
        part2 = (solutionPart2, [fileData realInput `ShouldBe` fileDataM Right realOutput])
      }
