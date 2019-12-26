module Day2
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram hiding (showResult)

replaceAt :: Int -> [String] -> [String] -> [String]
replaceAt pos values list =
  let (upper, lower) = splitAt pos list
      suffix = drop (length values) lower
   in upper ++ values ++ suffix

runGravityAssistProgram :: [String] -> Either String Int
runGravityAssistProgram prog =
  case completedProgram of
    Right (res:_) -> Right $ read res
    Left err      -> Left err
    _             -> Left "Illegal program state!"
  where
    completedProgram = program <$> runIntCodeProgram (programState prog)

findInputPairFor :: Int -> [String] -> Either String (Int, Int)
findInputPairFor targetOutput prog =
  case nounVerbPairs of
    []     -> Left $ "Could not find pair for: " ++ show targetOutput
    pair:_ -> Right pair
  where
    nounVerbPairs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99], targetOutput `elem` outputFor noun verb]
    outputFor noun verb = runGravityAssistProgram $ replaceAt 1 [show noun, show verb] prog

nounVerbChecksum :: (Int, Int) -> Int
nounVerbChecksum (noun, verb) = 100 * noun + verb

showResult :: Either String Int -> String
showResult (Left err)  = "Error: " ++ err
showResult (Right res) = show res

inputFile :: String
inputFile = "./resources/input-day2.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showResult . runGravityAssistProgram

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= putStrLn . showResult . fmap nounVerbChecksum . findInputPairFor 19690720
