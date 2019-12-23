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
runGravityAssistProgram [] = Left "Program is missing!"
runGravityAssistProgram prog =
  case completedProgram of
    Right (res:_) -> Right $ read res
    Left err      -> Left err
    _             -> Left "Illegal program state!"
  where
    completedProgram = program <$> runIntCodeProgram (ProgramState 0 [] prog 0 0 False False)

findInputPairFor :: Int -> [String] -> Either String (Int, Int)
findInputPairFor output prog =
  case nounVerbPairs of
    []     -> Left $ "Could not find pair for: " ++ show output
    pair:_ -> Right pair
  where
    nounVerbPairs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99], output `elem` computeOutputFor noun verb]
    computeOutputFor noun verb = runGravityAssistProgram $ replaceAt 1 [show noun, show verb] prog

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
