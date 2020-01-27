module Day16Test
  ( test
  ) where

import           Day16
import           UnitTest

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
      { day = 16
      , part1 =
          ( solutionPart1
          , [ Assertion (Constant testCase1) (Constant 24176176)
            , Assertion (Constant testCase2) (Constant 73745418)
            , Assertion (Constant testCase3) (Constant 52432133)
            , Assertion (fileSource realInput) (Constant 84487724)
            ])
      , part2 =
          ( solutionPart2
          , [ Assertion (Constant testCase4) (Constant 84462026)
            , Assertion (Constant testCase5) (Constant 78725270)
            , Assertion (Constant testCase6) (Constant 53553731)
            , Assertion (fileSource realInput) (Constant 84692524)
            ])
      }
