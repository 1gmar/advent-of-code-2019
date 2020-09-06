module Main (main) where

import qualified Day10Test as Day10
import qualified Day11Test as Day11
import qualified Day12Test as Day12
import qualified Day13Test as Day13
import qualified Day14Test as Day14
import qualified Day15Test as Day15
import qualified Day16Test as Day16
import qualified Day17Test as Day17
import qualified Day1Test as Day1
import qualified Day2Test as Day2
import qualified Day3Test as Day3
import qualified Day4Test as Day4
import qualified Day5Test as Day5
import qualified Day6Test as Day6
import qualified Day7Test as Day7
import qualified Day8Test as Day8
import qualified Day9Test as Day9

main :: IO ()
main =
  putStrLn "Running AoC tests:"
    >> sequence_
      [ Day1.test,
        Day2.test,
        Day3.test,
        Day4.test,
        Day5.test,
        Day6.test,
        Day7.test,
        Day8.test,
        Day9.test,
        Day10.test,
        Day11.test,
        Day12.test,
        Day13.test,
        Day14.test,
        Day15.test,
        Day16.test,
        Day17.test
      ]
