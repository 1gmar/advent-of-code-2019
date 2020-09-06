module Day9Test
  ( test,
  )
where

import Day9
import Util.UnitTest

testCase :: String
testCase = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

realInput :: String
realInput = "./resources/input/day9.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 9,
        part1 =
          ( solutionPart1,
            [ Assertion (Constant testCase) (Constant $ return 99),
              Assertion (Constant "1102,34915192,34915192,7,4,7,99,0") (Constant $ return 1219070632396864),
              Assertion (Constant "104,1125899906842624,99") (Constant $ return 1125899906842624),
              Assertion (fileSource realInput) (Constant $ return 2752191671)
            ]
          ),
        part2 = (solutionPart2, [Assertion (fileSource realInput) (Constant $ return 87571)])
      }
