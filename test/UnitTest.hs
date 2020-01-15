{-# LANGUAGE RecordWildCards #-}

module UnitTest
  ( Assertion(..)
  , Input(Constant)
  , DayTest(..)
  , runTest
  , fileInput
  ) where

import           Control.Exception (AssertionFailed (..), throwIO)
import           Control.Monad     (void)

data Input a
  = File String (IO a)
  | Constant a

instance Show a => Show (Input a) where
  show (File file _)       = file
  show (Constant constant) = show constant

data Assertion a b =
  Assertion
    { input    :: Input a
    , run      :: a -> b
    , expected :: b
    }

data DayTest a b =
  DayTest
    { day       :: Int
    , testCases :: ([Assertion a b], [Assertion a b])
    }

fileInput :: String -> Input String
fileInput file = File file (readFile file)

apply :: (a -> b) -> Input a -> IO b
apply f input =
  case input of
    File _ inputIO    -> f <$> inputIO
    Constant constant -> pure (f constant)

assert :: (Eq b, Show a, Show b) => Assertion a b -> IO ()
assert Assertion {..} = do
  putStrLn $ "Test Case:\n" ++ show input
  result <- run `apply` input
  reportTestResult result
  where
    errorMsg result = concat ["AssertionFailed: expected: ", show expected, ", but got: ", show result]
    reportTestResult result
      | result == expected = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runTest :: (Eq b, Show a, Show b) => DayTest a b -> IO ()
runTest DayTest {..} = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  mapM_ assert (fst testCases)
  putStrLn "Part 2:\n"
  mapM_ assert (snd testCases)
