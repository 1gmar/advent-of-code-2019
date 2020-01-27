{-# LANGUAGE RecordWildCards #-}

module IntCodeProgram
  ( ProgramState(input, result, halted, iPointer)
  , ProgramResult
  , runIntCodeProgram
  , programState
  , programWithInput
  , programWithOutput
  , parseIntCode
  , programList
  , outputList
  ) where

import           Data.Bifunctor              (bimap)
import           Data.List                   (find, unfoldr)
import           Data.Vector.Unboxed         (Vector, empty, fromList, modify, slice, snoc, toList, (!))
import qualified Data.Vector.Unboxed         as V (length, null, replicate, (++))
import           Data.Vector.Unboxed.Mutable (write)
import           ParseUtils

type ProgramResult = Either String ProgramState

type Parameter = (ParamMode, Int)

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

type IntV = Vector Int

data ParamMode
  = Position
  | Immediate
  | Relative

data Operation
  = Evaluate Operator
  | Input
  | Output
  | JumpIf Predicate
  | UpdateRelBase

data ProgramState =
  ProgramState
    { iPointer     :: Int
    , halted       :: Bool
    , input        :: [Int]
    , output       :: IntV
    , program      :: IntV
    , relativeBase :: Int
    , result       :: Int
    }

data Instruction =
  Instruction
    { operation :: Operation
    , params    :: [Parameter]
    }

argsLenToInstrCode :: [(Int, [Int])]
argsLenToInstrCode = [(1, [3, 4, 9]), (2, [5, 6]), (3, [1, 2, 7, 8])]

toParamMode :: Int -> Either String ParamMode
toParamMode mode =
  case mode of
    0 -> Right Position
    1 -> Right Immediate
    2 -> Right Relative
    _ -> Left $ "Unknown parameter mode: " ++ show mode

toOperation :: Int -> Either String Operation
toOperation opCode =
  case opCode of
    1 -> Right $ Evaluate (+)
    2 -> Right $ Evaluate (*)
    3 -> Right Input
    4 -> Right Output
    5 -> Right $ JumpIf (/= 0)
    6 -> Right $ JumpIf (== 0)
    7 -> Right $ Evaluate (assert (<))
    8 -> Right $ Evaluate (assert (==))
    9 -> Right UpdateRelBase
    _ -> Left $ "Unknown operation code: " ++ show opCode
  where
    assert condition arg1 arg2
      | arg1 `condition` arg2 = 1
      | otherwise = 0

nextIPointer :: Operation -> Int -> Int
nextIPointer op =
  case op of
    Evaluate _    -> (+ 4)
    Input         -> (+ 2)
    Output        -> (+ 2)
    JumpIf _      -> (+ 3)
    UpdateRelBase -> (+ 2)

maybeGrow :: IntV -> Int -> Either String IntV
maybeGrow program index
  | 0 <= index = Right extendedProg
  | otherwise = Left $ "Invalid negative instruction pointer: " ++ show index
  where
    progSize = V.length program
    extendedProg =
      if index < progSize
        then program
        else program V.++ V.replicate (index - progSize + 1) 0

elemAt :: IntV -> Int -> Either String (Int, IntV)
elemAt program index = getElem <$> maybeGrow program index
  where
    getElem prog = (prog ! index, prog)

replaceAt :: Int -> Int -> IntV -> Either String IntV
replaceAt index value program = modify writeValue <$> maybeGrow program index
  where
    writeValue vector = write vector index value

tokenize :: Int -> [Int]
tokenize 0 = [0]
tokenize intCode = unfoldr maybeDiv intCode
  where
    maybeDiv instr
      | instr > 0 = Just (instr `mod` 10, instr `div` 10)
      | otherwise = Nothing

paramOf :: Parameter -> ProgramState -> Either String (Int, IntV)
paramOf (mode, param) ProgramState {..} =
  case mode of
    Immediate -> Right (param, program)
    Position  -> program `elemAt` param
    Relative  -> program `elemAt` (relativeBase + param)

writeTo :: Parameter -> Int -> Int
writeTo (Relative, resultP) relBase = resultP + relBase
writeTo (_, resultP) _              = resultP

endOfProgram :: IntV -> Int -> Bool
endOfProgram program iPointer = 99 `elem` (fst <$> program `elemAt` iPointer)

returnState :: ProgramState -> Bool -> ProgramResult
returnState state@ProgramState {..} isHalted = Right state {halted = isHalted}

readInput :: ProgramState -> Instruction -> ProgramResult
readInput ProgramState {..} (Instruction _ []) = illegalProgramState iPointer program
readInput state@ProgramState {..} (Instruction op (param:_)) =
  case input of
    []         -> returnState state False
    value:rest -> replaceAt resultP value program >>= runIntCodeProgram . nextState rest
  where
    nextP = nextIPointer op iPointer
    resultP = writeTo param relativeBase
    nextState rest prog = state {iPointer = nextP, input = rest, program = prog}

writeOutput :: ProgramState -> Instruction -> ProgramResult
writeOutput state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> withParam param >>= runIntCodeProgram . nextState
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    withParam param = paramOf param state
    nextState (value, prog) = state {iPointer = nextP, output = output `snoc` value, program = prog, result = value}

jumpToPointerIf :: ProgramState -> Instruction -> Predicate -> ProgramResult
jumpToPointerIf state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (param1:param2:_) -> withParam param1 >>= chooseIPointer param2 >>= jumpToAddress
    _                               -> illegalProgramState iPointer program
  where
    jumpToAddress (pointer, prog) = runIntCodeProgram state {iPointer = pointer, program = prog}
    withParam param = paramOf param state
    chooseIPointer param (value, prog)
      | predicate value = paramOf param state {program = prog}
      | otherwise = Right (nextIPointer operation iPointer, prog)

computeValue :: ProgramState -> Instruction -> Operator -> ProgramResult
computeValue state@ProgramState {..} instr@Instruction {..} operator =
  case instr of
    Instruction _ (p1:p2:p3:_) -> evaluateFor (p1, p2, p3)
    _                          -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    evaluateFor (p1, p2, p3) = do
      (arg1, prog1) <- paramOf p1 state
      (arg2, prog2) <- paramOf p2 state {program = prog1}
      let resultP = writeTo p3 relativeBase
      prog3 <- replaceAt resultP (arg1 `operator` arg2) prog2
      runIntCodeProgram state {iPointer = nextP, program = prog3}

updateRelativeBase :: ProgramState -> Instruction -> ProgramResult
updateRelativeBase state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> paramOf param state >>= runIntCodeProgram . updateBase
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    updateBase = nextState . bimap (+ relativeBase) id
    nextState (base, prog) = state {iPointer = nextP, program = prog, relativeBase = base}

illegalProgramState :: Int -> IntV -> ProgramResult
illegalProgramState iPointer program = Left $ "Illegal program state at: " ++ show (iPointer, program)

runInstruction :: ProgramState -> Instruction -> ProgramResult
runInstruction state instr@Instruction {..} =
  case operation of
    Input            -> readInput state instr
    Output           -> writeOutput state instr
    JumpIf predicate -> jumpToPointerIf state instr predicate
    Evaluate op      -> computeValue state instr op
    UpdateRelBase    -> updateRelativeBase state instr

buildInstruction :: [Int] -> [Int] -> Either String Instruction
buildInstruction instrTokens args =
  case instrTokens of
    [opCode]                               -> instructionWith opCode zeros
    [opCode, 0]                            -> instructionWith opCode zeros
    [opCode, 0, pmCode]                    -> instructionWith opCode (pmCode : zeros)
    [opCode, 0, pmCode1, pmCode2]          -> instructionWith opCode (pmCode1 : pmCode2 : zeros)
    [opCode, 0, pmCode1, pmCode2, pmCode3] -> instructionWith opCode (pmCode1 : pmCode2 : pmCode3 : zeros)
    _                                      -> Left $ "Invalid instruction format: " ++ show instrTokens
  where
    zeros = replicate (length args) 0
    instructionWith opCode pmCodes = do
      paramModes <- mapM toParamMode pmCodes
      op <- toOperation opCode
      Right $ Instruction op (paramModes `zip` args)

processInstruction :: ProgramState -> (Int, IntV) -> ProgramResult
processInstruction state@ProgramState {..} (instr, newProg) =
  case fst <$> instrCodeMatch of
    Just argsLen -> buildInstruction tokens (args argsLen) >>= runInstruction nextState
    Nothing      -> Left $ "Unknown instruction: " ++ show instr
  where
    args argsLen = toList $ slice (iPointer + 1) argsLen newProg
    nextState = state {program = newProg}
    tokens@(instrCode:_) = tokenize instr
    instrCodeMatch = find ((instrCode `elem`) . snd) argsLenToInstrCode

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram state@ProgramState {..}
  | V.null program = Left "Program is missing!"
  | endOfProgram program iPointer = returnState state True
  | otherwise = program `elemAt` iPointer >>= processInstruction state

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

programList :: ProgramState -> [Int]
programList = toList . program

outputList :: ProgramState -> [Int]
outputList = toList . output

programWithOutput :: ProgramState -> [Int] -> ProgramState
programWithOutput progState outData = progState {output = fromList outData}

programState :: [Int] -> ProgramState
programState prog = ProgramState 0 False [] empty (fromList prog) 0 0

programWithInput :: [Int] -> [Int] -> ProgramState
programWithInput prog inputData = (programState prog) {input = inputData}
