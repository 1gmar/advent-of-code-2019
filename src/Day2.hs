module Day2
  ( solutionPart1,
    solutionPart2,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Util.IntCodeProgram

replaceAt :: Int -> [Int] -> [Int] -> [Int]
replaceAt pos values list =
  let (upper, lower) = splitAt pos list
      suffix = drop (length values) lower
   in upper ++ values ++ suffix

runGravityAssistProgram :: [Int] -> Either String Int
runGravityAssistProgram prog =
  case completedProgram of
    Right (res : _) -> return res
    Left err -> throwError err
    _ -> throwError "Illegal program state!"
  where
    completedProgram = programMemory <$> runIntCodeProgram (newProgram prog)

findInputPairFor :: Int -> [Int] -> Either String (Int, Int)
findInputPairFor targetOutput prog =
  case nounVerbPairs of
    [] -> Left $ "Could not find pair for: " ++ show targetOutput
    pair : _ -> Right pair
  where
    nounVerbPairs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99], targetOutput `elem` outputFor noun verb]
    outputFor noun verb = runGravityAssistProgram $ replaceAt 1 [noun, verb] prog

nounVerbChecksum :: (Int, Int) -> Int
nounVerbChecksum (noun, verb) = 100 * noun + verb

solutionPart1 :: String -> Either String Int
solutionPart1 = runGravityAssistProgram . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = fmap nounVerbChecksum . findInputPairFor 19690720 . parseIntCode
