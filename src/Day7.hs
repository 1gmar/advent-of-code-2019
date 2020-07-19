{-# LANGUAGE RecordWildCards #-}

module Day7
  ( solutionPart1
  , solutionPart2
  ) where

import           Control.Monad       (foldM)
import           Data.List           (maximumBy, permutations)
import           Data.Traversable    (for)
import           Util.IntCodeProgram

type AmpChainRunner = [Int] -> [Int] -> ProgramResult

data Amplifier =
  Amplifier
    { label     :: Char
    , softState :: ProgramState
    }

runSimpleChain :: AmpChainRunner
runSimpleChain initProg = foldM chainResult (programState initProg)
  where
    chainResult state phase = runIntCodeProgram $ programWithInput initProg [phase, result state]

runLoopModeChain :: AmpChainRunner
runLoopModeChain initProg phaseSetting = loopOver amplifiers
  where
    amplifiers = zipWith buildAmplifier ['A' .. 'E'] phaseSetting
    buildAmplifier label phase = Amplifier label $ programWithInput initProg [phase]

loopOver :: [Amplifier] -> ProgramResult
loopOver [] = Left "Missing amplifier chain."
loopOver [_] = Left "Illegal amplifier setup."
loopOver (current@(Amplifier code state):next@Amplifier {..}:rest)
  | code == 'E' && halted state = Right state
  | code == 'A' && iPointer state == 0 = runIntCodeProgram (inSignal 0 state) >>= chainResult current
  | otherwise = runIntCodeProgram state >>= chainResult current
  where
    chainResult amp nextState = loopOver (setupNext (result nextState) : rest ++ [amp {softState = nextState}])
    setupNext res = next {softState = inSignal res softState}
    inSignal signal nextState = nextState {input = input nextState ++ [signal]}

findMaxPossibleSignal :: AmpChainRunner -> [[Int]] -> [Int] -> ProgramResult
findMaxPossibleSignal runner allPhaseSeq = fmap (maximumBy compareStates) . for allPhaseSeq . runner
  where
    compareStates state1 state2 = result state1 `compare` result state2

solutionPart1 :: String -> Either String Int
solutionPart1 = fmap result . findMaxPossibleSignal runSimpleChain (permutations [0 .. 4]) . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = fmap result . findMaxPossibleSignal runLoopModeChain (permutations [5 .. 9]) . parseIntCode
