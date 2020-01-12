{-# LANGUAGE RecordWildCards #-}

module AssertUtils
  ( Assertion(..)
  , Source(..)
  , runAssertions
  ) where

import           Control.Exception (AssertionFailed (..), throwIO)
import           Control.Monad     (void)

type Day = Int

data Source a b
  = File (String -> b) String
  | Raw (a -> b) a

instance (Show a, Show b) => Show (Source a b) where
  show (File _ file) = file
  show (Raw _ input) = show input

data Assertion a b =
  Assertion
    { source   :: Source a b
    , expected :: b
    }

solution :: Source a b -> IO b
solution source =
  case source of
    File f file -> f <$> readFile file
    Raw f input -> pure (f input)

assert :: (Eq b, Show a, Show b) => Assertion a b -> IO ()
assert Assertion {..} = do
  result <- solution source
  putStrLn $ "Test Case:\n" ++ show source
  reportTestResult result
  where
    errorMsg result = concat ["Failed: expected: ", show expected, ", but got: ", show result]
    reportTestResult result
      | result == expected = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runAssertions :: (Eq b, Show a, Show b) => Day -> [Assertion a b] -> [Assertion a b] -> IO ()
runAssertions day part1Assertions part2Assertions = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  mapM_ assert part1Assertions
  putStrLn "Part 2:\n"
  mapM_ assert part2Assertions
