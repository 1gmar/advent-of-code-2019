{-# LANGUAGE RecordWildCards #-}

module Day7
  ( solutionPart1
  , solutionPart2
  ) where

import           Control.Monad    (foldM)
import           Data.List        (maximumBy, permutations)
import           Data.Traversable (for)
import           IntCodeProgram

type AmpChainRunner = [String] -> [Int] -> ProgramResult

data Amplifier =
  Amplifier
    { label     :: Char
    , softState :: ProgramState
    }

runSimpleChain :: [String] -> [Int] -> ProgramResult
runSimpleChain initProg = foldM chainResult (ProgramState 0 [] [] 0 0 False False)
  where
    chainResult ProgramState {..} phase = runIntCodeProgram $ ProgramState 0 [phase, result] initProg 0 0 False False

findMaxPossibleSignal :: AmpChainRunner -> [[Int]] -> [String] -> ProgramResult
findMaxPossibleSignal runner allPhaseSeq = fmap (maximumBy compareStates) . for allPhaseSeq . runner
  where
    compareStates (ProgramState _ _ _ res1 _ _ _) (ProgramState _ _ _ res2 _ _ _) = res1 `compare` res2

runLoopModeChain :: [String] -> [Int] -> ProgramResult
runLoopModeChain initProg phaseSetting = loopOver amplifiers
  where
    amplifiers = zipWith buildAmplifier ['A' .. 'E'] phaseSetting
    buildAmplifier label phase = Amplifier label $ ProgramState 0 [phase] initProg 0 0 True False

loopOver :: [Amplifier] -> ProgramResult
loopOver [] = Left "Missing amplifier chain."
loopOver [_] = Left "Illegal amplifier setup."
loopOver (current:next@Amplifier {..}:rest) =
  case current of
    Amplifier 'E' state@(ProgramState _ _ _ _ _ _ True) -> Right state
    amp@(Amplifier 'A' state@(ProgramState 0 _ _ _ _ _ _)) -> runIntCodeProgram (inSignal 0 state) >>= chainResult amp
    amp@(Amplifier _ state) -> runIntCodeProgram state >>= chainResult amp
  where
    chainResult amp state@(ProgramState _ _ _ res _ _ _) = loopOver (setupNext res : rest ++ [amp {softState = state}])
    setupNext res = next {softState = inSignal res softState}
    inSignal signal state@ProgramState {..} = state {input = input ++ [signal]}

inputFile :: String
inputFile = "./resources/input-day7.txt"

solutionPart1 :: IO ()
solutionPart1 =
  readInputData inputFile >>= putStrLn . showResult . findMaxPossibleSignal runSimpleChain (permutations [0 .. 4])

solutionPart2 :: IO ()
solutionPart2 =
  readInputData inputFile >>= putStrLn . showResult . findMaxPossibleSignal runLoopModeChain (permutations [5 .. 9])
