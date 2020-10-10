{-# LANGUAGE GADTs #-}

module Util.UnitTest
  ( Assertion (..),
    Data (Const),
    DayTest (..),
    runTest,
    fileData,
    fileDataM,
  )
where

import Control.Exception (AssertionFailed (..), throwIO)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)

data Data a
  = File String (IO a)
  | Const a

instance (Show a, Typeable a) => Show (Data a) where
  show = \case
    File file _ -> file
    Const constant -> fromMaybe (show constant) (cast constant :: Maybe String)

data Assertion a b where
  ShouldBe :: (Eq a, Eq b, Show a, Show b, Typeable a, Typeable b) => Data a -> Data b -> Assertion a b

data DayTest a1 a2 b1 b2 = DayTest
  { day :: Int,
    part1 :: (a1 -> b1, [Assertion a1 b1]),
    part2 :: (a2 -> b2, [Assertion a2 b2])
  }

fileData :: String -> Data String
fileData file = File file (readFile file)

fileDataM :: (String -> m) -> String -> Data m
fileDataM fM file = File file (fM <$> readFile file)

readData :: Data a -> IO a
readData = \case
  File _ dataIO -> dataIO
  Const constant -> pure constant

assert :: (a -> b) -> Assertion a b -> IO ()
assert solution (ShouldBe input expected) = do
  putStrLn $ "Test Case:\n" ++ show input
  result <- solution <$> readData input
  (result `shouldBe`) =<< readData expected
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
