module Day1Test
  ( test
  ) where

import           Day1
import           UnitTest

realInput :: String
realInput = "./resources/input/day1.txt"

test :: IO ()
test =
  runTest
    DayTest
      { day = 1
      , part1 =
          ( solutionPart1
          , [ Assertion (Constant "12") (Constant 2)
            , Assertion (Constant "14") (Constant 2)
            , Assertion (Constant "1969") (Constant 654)
            , Assertion (Constant "100756") (Constant 33583)
            , Assertion (fileSource realInput) (Constant 3423511)
            ])
      , part2 =
          ( solutionPart2
          , [ Assertion (Constant "12") (Constant 2)
            , Assertion (Constant "14") (Constant 2)
            , Assertion (Constant "1969") (Constant 966)
            , Assertion (Constant "100756") (Constant 50346)
            , Assertion (fileSource realInput) (Constant 5132379)
            ])
      }
