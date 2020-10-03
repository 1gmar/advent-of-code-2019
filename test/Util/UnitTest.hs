{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.UnitTest
  ( Assertion (..),
    Source (Constant),
    DayTest (..),
    runTest,
    fileSource,
    fileSourceM,
  )
where

import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (void)
import Data.Typeable (Typeable, cast)

data Source a where
  File :: String -> IO a -> Source a
  Constant :: (Eq a, Show a, Typeable a) => a -> Source a

instance Show (Source a) where
  show (File file _) = file
  show (Constant constant) = case cast constant of
    Just (str :: String) -> str
    Nothing -> show constant

data Assertion a b where
  Assertion :: (Eq a, Eq b, Show a, Show b, Typeable a, Typeable b) => Source a -> Source b -> Assertion a b

data DayTest a1 a2 b1 b2 = DayTest
  { day :: Int,
    part1 :: (a1 -> b1, [Assertion a1 b1]),
    part2 :: (a2 -> b2, [Assertion a2 b2])
  }

fileSource :: String -> Source String
fileSource file = File file (readFile file)

fileSourceM :: (String -> m) -> String -> Source m
fileSourceM fM file = File file (fM <$> readFile file)

readSource :: Source a -> IO a
readSource = \case
  File _ sourceIO -> sourceIO
  Constant constant -> pure constant

assert :: (a -> b) -> Assertion a b -> IO ()
assert solution (Assertion input expected) = do
  putStrLn $ "Test Case:\n" ++ show input
  result <- solution <$> readSource input
  (result `shouldBe`) =<< readSource expected
  where
    errorMsg result = concat ["AssertionFailed: expected: ", show expected, ", but got: ", show result]
    shouldBe result expect
      | result == expect = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runTest :: DayTest a1 a2 b1 b2 -> IO ()
runTest DayTest {..} = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  runAssertions part1
  putStrLn "Part 2:\n"
  runAssertions part2
  where
    runAssertions (solution, testCases) = mapM_ (assert solution) testCases
