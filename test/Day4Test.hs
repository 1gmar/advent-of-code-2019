module Day4Test
  ( runTests
  ) where

import           AssertUtils
import           Day4

runTests :: IO ()
runTests =
  runAssertions
    4
    [Assertion (Raw solutionPart1 (134792, 675810)) 1955]
    [Assertion (Raw solutionPart2 (134792, 675810)) 1319]
