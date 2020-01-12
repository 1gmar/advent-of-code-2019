{-# LANGUAGE RecordWildCards #-}

module AssertUtils
  ( AssertData(..)
  , InputType(..)
  , runAssertions
  ) where

import           Control.Exception (AssertionFailed (..), throwIO)
import           Control.Monad     (void)

type Day = Int

data InputType
  = Raw
  | File

data AssertData a =
  AssertData
    { inputType :: InputType
    , solution  :: String -> a
    , input     :: String
    , expected  :: a
    }

assert :: (Eq a, Show a) => AssertData a -> IO ()
assert AssertData {..} = do
  result <- solution <$> inputData
  putStrLn $ "Test Case: \n" ++ input
  reportTestResult result
  where
    errorMsg result = concat ["Failed: expected: ", show expected, ", but got: ", show result]
    inputData =
      case inputType of
        File -> readFile input
        Raw  -> pure input
    reportTestResult result
      | result == expected = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runAssertions :: (Eq a, Show a) => Day -> [AssertData a] -> [AssertData a] -> IO ()
runAssertions day part1Assertions part2Assertions = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  mapM_ assert part1Assertions
  putStrLn "Part 2:\n"
  mapM_ assert part2Assertions
