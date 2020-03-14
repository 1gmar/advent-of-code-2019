{-# LANGUAGE RecordWildCards #-}

module Util.UnitTest
  ( Assertion(..)
  , Source(Constant)
  , DayTest(..)
  , runTest
  , fileSource
  , fileSourceM
  ) where

import           Control.Exception (AssertionFailed (..), throwIO)
import           Control.Monad     (void)

data Source a
  = File String (IO a)
  | Constant a

instance Show a => Show (Source a) where
  show (File file _)       = file
  show (Constant constant) = show constant

data Assertion a b =
  Assertion
    { input    :: Source a
    , expected :: Source b
    }

data DayTest a1 a2 b1 b2 =
  DayTest
    { day   :: Int
    , part1 :: (a1 -> b1, [Assertion a1 b1])
    , part2 :: (a2 -> b2, [Assertion a2 b2])
    }

fileSource :: String -> Source String
fileSource file = File file (readFile file)

fileSourceM :: (String -> m) -> String -> Source m
fileSourceM fM file = File file (fM <$> readFile file)

readSource :: Source a -> IO a
readSource source =
  case source of
    File _ sourceIO   -> sourceIO
    Constant constant -> pure constant

assert :: (Eq b, Show a, Show b) => (a -> b) -> Assertion a b -> IO ()
assert run Assertion {..} = do
  putStrLn $ "Test Case:\n" ++ show input
  result <- run <$> readSource input
  (result `shouldBe`) =<< readSource expected
  where
    errorMsg result = concat ["AssertionFailed: expected: ", show expected, ", but got: ", show result]
    shouldBe result expect
      | result == expect = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runTest :: (Eq b1, Eq b2, Show a1, Show a2, Show b1, Show b2) => DayTest a1 a2 b1 b2 -> IO ()
runTest DayTest {..} = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  runAssertions part1
  putStrLn "Part 2:\n"
  runAssertions part2
  where
    runAssertions (run, testCases) = mapM_ (assert run) testCases
