module Day10Test
  ( test,
  )
where

import Day10
import Util.UnitTest

realInput :: String
realInput = "./resources/input/day10.txt"

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
      { day = 10,
        part1 =
          ( solutionPart1,
            [ Const testCase1 `ShouldBe` Const 8,
              Const testCase2 `ShouldBe` Const 33,
              Const testCase3 `ShouldBe` Const 35,
              Const testCase4 `ShouldBe` Const 41,
              Const testCase5 `ShouldBe` Const 210,
              fileData realInput `ShouldBe` Const 347
            ]
          ),
        part2 =
          ( solutionPart2,
            [Const testCase5 `ShouldBe` Const 802, fileData realInput `ShouldBe` Const 829]
          )
      }
