module Day2Test
  ( test,
  )
where

import Day2
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day2.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 2,
        part1 =
          ( solutionPart1,
            [ Const "1,9,10,3,2,3,11,0,99,30,40,50" `ShouldBe` Const (Right 3500),
              Const "1,0,0,0,99" `ShouldBe` Const (Right 2),
              Const "1,1,1,4,99,5,6,0,99" `ShouldBe` Const (Right 30),
              fileData realInput `ShouldBe` Const (Right 3716293)
            ]
          ),
        part2 = (solutionPart2, [fileData realInput `ShouldBe` Const (Right 6429)])
      }
