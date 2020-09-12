module Util.IntCodeProgram
  ( ProgramState (input, result, halted, iPointer),
    ProgramResult,
    newProgram,
    programWithInput,
    programWithOutput,
    parseIntCode,
    programMemory,
    outputList,
    runIntCodeProgram,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.ST (runST)
import Control.Monad.State (StateT, execStateT, get, gets, modify')
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
import qualified Data.List.NonEmpty as NE (init)
import Data.Tuple (swap)
import Data.Vector.Unboxed
  ( Vector,
    fromList,
    modify,
    slice,
    toList,
    unsafeFreeze,
    unsafeThaw,
    (!),
  )
import qualified Data.Vector.Unboxed as V (length, null)
import Data.Vector.Unboxed.Mutable (grow, write)
import Util.ParseUtils

type IntCodeProgram = StateT ProgramState (Either String)

type ProgramResult = Either String ProgramState

type Parameter = (Int, ParamMode)

type ParamT2 = (Parameter, Parameter)

type ParamT3 = (Parameter, Parameter, Parameter)

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

type IntV = Vector Int

type ArgCount = Int

type EndRoutine = IntCodeProgram ()

data ParamMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data Instruction
  = Input !Parameter
  | Output !Parameter
  | UpdateRelBase !Parameter
  | JumpEQZ !ParamT2
  | JumpNEZ !ParamT2
  | Add !ParamT3
  | Mult !ParamT3
  | IfLess !ParamT3
  | IfEqual !ParamT3
  deriving (Show)

data ProgramState = ProgramState
  { iPointer :: !Int,
    halted :: !Bool,
    input :: ![Int],
    output :: [Int],
    memory :: !IntV,
    relativeBase :: !Int,
    result :: !Int
  }

setMemory :: ProgramState -> IntV -> ProgramState
setMemory state memory = state {memory = memory}

instructionRegistry :: [(ArgCount, [Int])]
instructionRegistry =
  [ (1, [3 {- Input -}, 4 {- Output -}, 9 {- UpdateRelBase -}]),
    (2, [5 {- JumpNEZ -}, 6 {- JumpEQZ -}]),
    (3, [1 {- Add -}, 2 {- Mult -}, 7 {- IfLess -}, 8 {- IfEqual -}])
  ]

tokenize :: Int -> NonEmpty Int
tokenize 0 = 0 :| []
tokenize intCode = unfoldr maybeDiv intCode
  where
    maybeDiv instr
      | instr > 0 = Just <$> swap (divMod instr 10)
      | otherwise = (instr, Nothing)

illegalState :: Show a => String -> a -> IntCodeProgram b
illegalState message instr =
  gets iPointer >>= \i -> throwError $ concat [message, show instr, " at index: ", show i]

toParamMode :: Int -> IntCodeProgram ParamMode
toParamMode mode =
  case mode of
    0 -> return Position
    1 -> return Immediate
    2 -> return Relative
    _ -> illegalState "Invalid parameter mode: " mode

moveIPointerBy :: Int -> IntCodeProgram ()
moveIPointerBy value = modify' updateIPointer
  where
    updateIPointer state@ProgramState {..} = state {iPointer = iPointer + value}

allocateMemIfNeeded :: Int -> IntCodeProgram ()
allocateMemIfNeeded index
  | 0 <= index = getMemory
  | otherwise = illegalState "Invalid negative instruction pointer: " index
  where
    allocMemory currentMem newSize = runST $ unsafeThaw currentMem >>= (`grow` newSize) >>= unsafeFreeze
    getMemory = do
      memVector <- gets memory
      let memSize = V.length memVector
      when (memSize <= index) $ modify' (`setMemory` allocMemory memVector (index - memSize + 1))

elemAt :: Int -> IntCodeProgram Int
elemAt index = allocateMemIfNeeded index >> gets ((! index) . memory)

replaceAt :: Int -> Int -> IntCodeProgram ()
replaceAt index value = allocateMemIfNeeded index >> modify' updateMemory
  where
    writeValue vector = write vector index value
    updateMemory state@ProgramState {..} = setMemory state $ modify writeValue memory

valueOf :: Parameter -> IntCodeProgram Int
valueOf (param, mode) =
  case mode of
    Immediate -> return param
    Position -> elemAt param
    Relative -> gets relativeBase >>= elemAt . (+ param)

indexOf :: Parameter -> IntCodeProgram Int
indexOf (resultP, mode) =
  case mode of
    Relative -> gets ((+ resultP) . relativeBase)
    _ -> return resultP

yieldState :: Bool -> IntCodeProgram ()
yieldState isHalted = modify' setHalted
  where
    setHalted state = state {halted = isHalted}

readInput :: EndRoutine -> Parameter -> IntCodeProgram ()
readInput end parameter =
  gets input
    >>= ( \case
            [] -> yieldState False
            value : rest -> processInput value rest
        )
  where
    setInput value state = state {input = value}
    processInput value rest = do
      resultP <- indexOf parameter
      replaceAt resultP value
      modify' (setInput rest)
      end

writeOutput :: EndRoutine -> Parameter -> IntCodeProgram ()
writeOutput end parameter = do
  value <- valueOf parameter
  modify' (appendOutput value)
  modify' (setResult value)
  end
  where
    setResult value state = state {result = value}
    appendOutput value state@ProgramState {..} = state {output = value : output}

jumpToPointerIf :: Predicate -> EndRoutine -> ParamT2 -> IntCodeProgram ()
jumpToPointerIf predicate end (param1, param2) = valueOf param1 >>= selectEndRoutine
  where
    setIPointer value state = state {iPointer = value}
    selectEndRoutine value
      | predicate value = valueOf param2 >>= modify' . setIPointer >> programStep
      | otherwise = end

computeValue :: Operator -> EndRoutine -> ParamT3 -> IntCodeProgram ()
computeValue operator end (param1, param2, param3) = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  resultP <- indexOf param3
  replaceAt resultP (value1 `operator` value2)
  end

updateRelativeBase :: EndRoutine -> Parameter -> IntCodeProgram ()
updateRelativeBase end parameter = do
  value <- valueOf parameter
  modify' (updateBase value)
  end
  where
    updateBase value state@ProgramState {..} = state {relativeBase = relativeBase + value}

runInstruction :: (EndRoutine, Instruction) -> IntCodeProgram ()
runInstruction (end, instruction) =
  case instruction of
    Input param -> readInput end param
    Output param -> writeOutput end param
    UpdateRelBase param -> updateRelativeBase end param
    JumpNEZ params -> jumpToPointerIf (/= 0) end params
    JumpEQZ params -> jumpToPointerIf (== 0) end params
    IfLess params -> computeValue (assert (<)) end params
    IfEqual params -> computeValue (assert (==)) end params
    Add params -> computeValue (+) end params
    Mult params -> computeValue (*) end params
  where
    assert cmp lhs rhs
      | lhs `cmp` rhs = 1
      | otherwise = 0

buildInstruction :: [Int] -> [Int] -> IntCodeProgram (EndRoutine, Instruction)
buildInstruction instrTokens args =
  case instrTokens of
    [opCode] -> instructionWith opCode zeros
    [opCode, 0] -> instructionWith opCode zeros
    [opCode, 0, pmCode] -> instructionWith opCode (pmCode : zeros)
    [opCode, 0, pmCode1, pmCode2] -> instructionWith opCode (pmCode1 : pmCode2 : zeros)
    [opCode, 0, pmCode1, pmCode2, pmCode3] -> instructionWith opCode (pmCode1 : pmCode2 : pmCode3 : zeros)
    _ -> illegalState "Invalid instruction format: " instrTokens
  where
    zeros = replicate (length args) 0
    endRoutineFor argsLen = moveIPointerBy (argsLen + 1) >> programStep
    instructionWith opCode pmCodes = do
      paramModes <- mapM toParamMode pmCodes
      let endR = endRoutineFor $ length args
      (endR,) <$> newInstruction (opCode, args `zip` paramModes)

newInstruction :: (Int, [Parameter]) -> IntCodeProgram Instruction
newInstruction opCodeToParams =
  case opCodeToParams of
    (3, [param]) -> return $ Input param
    (4, [param]) -> return $ Output param
    (9, [param]) -> return $ UpdateRelBase param
    (5, [param1, param2]) -> return $ JumpNEZ (param1, param2)
    (6, [param1, param2]) -> return $ JumpEQZ (param1, param2)
    (1, [param1, param2, param3]) -> return $ Add (param1, param2, param3)
    (2, [param1, param2, param3]) -> return $ Mult (param1, param2, param3)
    (7, [param1, param2, param3]) -> return $ IfLess (param1, param2, param3)
    (8, [param1, param2, param3]) -> return $ IfEqual (param1, param2, param3)
    _ -> illegalState "Invalid instruction configuration: " opCodeToParams

processInstruction :: Int -> IntCodeProgram ()
processInstruction instr =
  case fst <$> maybeInstrPair of
    Just argsLen -> get >>= buildInstruction (NE.init tokens) . takeArgs argsLen >>= runInstruction
    Nothing -> illegalState "Illegal instruction: " instr
  where
    tokens@(opCode :| _) = tokenize instr
    maybeInstrPair = find (elem opCode . snd) instructionRegistry
    takeArgs argsLen ProgramState {..} = toList $ slice (iPointer + 1) argsLen memory

programStep :: IntCodeProgram ()
programStep = gets memory >>= stepInUnlessNull
  where
    stepInUnlessNull memory
      | V.null memory = illegalState "ProgramState memory is null: " memory
      | otherwise = gets iPointer >>= elemAt >>= stepIn
    stepIn instr
      | instr == 99 = yieldState True
      | otherwise = processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram = execStateT programStep

newProgram :: [Int] -> ProgramState
newProgram memory = ProgramState 0 False [] [] (fromList memory) 0 0

programMemory :: ProgramState -> [Int]
programMemory = toList . memory

outputList :: ProgramState -> [Int]
outputList = reverse . output

programWithInput :: ProgramState -> [Int] -> ProgramState
programWithInput state inputData = state {input = inputData}

programWithOutput :: ProgramState -> [Int] -> ProgramState
programWithOutput state outData = state {output = outData}
