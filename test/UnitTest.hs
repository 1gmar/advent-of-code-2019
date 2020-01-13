{-# LANGUAGE RecordWildCards #-}

module UnitTest
  ( Assertion(..)
  , Source(..)
  , DayTest(..)
  , runTest
  ) where

import           Control.Exception (AssertionFailed (..), throwIO)
import           Control.Monad     (void)

data Source a b
  = File (String -> b) String
  | Raw (a -> b) a

instance Show a => Show (Source a b) where
  show (File _ file) = file
  show (Raw _ input) = show input

data Assertion a b =
  Assertion
    { source   :: Source a b
    , expected :: b
    }

data DayTest a b =
  DayTest
    { day       :: Int
    , testCases :: ([Assertion a b], [Assertion a b])
    }

solution :: Source a b -> IO b
solution source =
  case source of
    File f file -> f <$> readFile file
    Raw f input -> pure (f input)

assert :: (Eq b, Show a, Show b) => Assertion a b -> IO ()
assert Assertion {..} = do
  putStrLn $ "Test Case:\n" ++ show source
  result <- solution source
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
