module Day1Spec
  ( runTests
  ) where

import           Day1

assert :: (String -> Int) -> String -> Int -> IO ()
assert solution input expected
  | result == expected = putStrLn $ "Passed for " ++ show input
  | otherwise =
    putStrLn $ concat ["Failed: expected: ", show expected, ", but got: ", show result, ", for input: ", show input]
  where
    result = solution input

runTests :: IO ()
runTests = do
  putStrLn "Day 1 test suite:"
  putStrLn "Part 1:"
  assert solutionPart1 "12" 2
  assert solutionPart1 "14" 2
  assert solutionPart1 "1969" 654
  assert solutionPart1 "100756" 33583
  readInput >>= flip (assert solutionPart1) 3423511
  putStrLn "Part 2:"
  assert solutionPart2 "14" 2
  assert solutionPart2 "1969" 966
  assert solutionPart2 "100756" 50346
  readInput >>= flip (assert solutionPart2) 5132379
