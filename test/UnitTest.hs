{-# LANGUAGE RecordWildCards #-}

module UnitTest
  ( Assertion(..)
  , Source(Constant)
  , DayTest(..)
  , runTest
  , fileSource
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
    , run      :: a -> b
    , expected :: Source b
    }

data DayTest a b c =
  DayTest
    { day       :: Int
    , testCases :: ([Assertion a b], [Assertion a c])
    }

fileSource :: String -> Source String
fileSource file = File file (readFile file)

readSource :: Source a -> IO a
readSource source =
  case source of
    File _ sourceIO   -> sourceIO
    Constant constant -> pure constant

assert :: (Eq b, Show a, Show b) => Assertion a b -> IO ()
assert Assertion {..} = do
  putStrLn $ "Test Case:\n" ++ show input
  result <- run <$> readSource input
  (result `shouldBe`) =<< readSource expected
  where
    errorMsg result = concat ["AssertionFailed: expected: ", show expected, ", but got: ", show result]
    shouldBe result expect
      | result == expect = putStrLn "Passed!\n"
      | otherwise = void $ throwIO $ AssertionFailed (errorMsg result)

runTest :: (Eq b, Eq c, Show a, Show b, Show c) => DayTest a b c -> IO ()
runTest DayTest {..} = do
  putStrLn $ "Day " ++ show day ++ " test suite:\n"
  putStrLn "Part 1:\n"
  mapM_ assert (fst testCases)
  putStrLn "Part 2:\n"
  mapM_ assert (snd testCases)
