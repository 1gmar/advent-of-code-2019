module Day10Test
  ( test
  ) where

import           Day10
import           UnitTest

realInput :: String
realInput = "./resources/input-day10.txt"

testCase1 :: String
testCase1 =
  ".#..#\n\
  \.....\n\
  \#####\n\
  \....#\n\
  \...##"

testCase2 :: String
testCase2 =
  "......#.#.\n\
  \#..#.#....\n\
  \..#######.\n\
  \.#.#.###..\n\
  \.#..#.....\n\
  \..#....#.#\n\
  \#..#....#.\n\
  \.##.#..###\n\
  \##...#..#.\n\
  \.#....####"

testCase3 :: String
testCase3 =
  "#.#...#.#.\n\
  \.###....#.\n\
  \.#....#...\n\
  \##.#.#.#.#\n\
  \....#.#.#.\n\
  \.##..###.#\n\
  \..#...##..\n\
  \..##....##\n\
  \......#...\n\
  \.####.###."

testCase4 :: String
testCase4 =
  ".#..#..###\n\
  \####.###.#\n\
  \....###.#.\n\
  \..###.##.#\n\
  \##.##.#.#.\n\
  \....###..#\n\
  \..#.#..#.#\n\
  \#..#.#.###\n\
  \.##...##.#\n\
  \.....#.#.."

testCase5 :: String
testCase5 =
  ".#..##.###...#######\n\
  \##.############..##.\n\
  \.#.######.########.#\n\
  \.###.#######.####.#.\n\
  \#####.##.#.##.###.##\n\
  \..#####..#.#########\n\
  \####################\n\
  \#.####....###.#.#.##\n\
  \##.#################\n\
  \#####.##.###..####..\n\
  \..######..##.#######\n\
  \####.##.####...##..#\n\
  \.#####..#.######.###\n\
  \##...#.##########...\n\
  \#.##########.#######\n\
  \.####.#.###.###.#.##\n\
  \....##.##.###..#####\n\
  \.#.#.###########.###\n\
  \#.#.#.#####.####.###\n\
  \###.##.####.##.#..##"

test :: IO ()
test =
  runTest
    DayTest
      { day = 10
      , testCases =
          ( [ Assertion (Constant testCase1) solutionPart1 (Constant 8)
            , Assertion (Constant testCase2) solutionPart1 (Constant 33)
            , Assertion (Constant testCase3) solutionPart1 (Constant 35)
            , Assertion (Constant testCase4) solutionPart1 (Constant 41)
            , Assertion (Constant testCase5) solutionPart1 (Constant 210)
            , Assertion (fileSource realInput) solutionPart1 (Constant 347)
            ]
          , [ Assertion (Constant testCase5) solutionPart2 (Constant 802)
            , Assertion (fileSource realInput) solutionPart2 (Constant 829)
            ])
      }
