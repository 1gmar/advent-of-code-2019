module Util.IntCodeProgram
  ( ProgramState (input, result, terminated, iPointer),
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

type InstrCode = Int

type ArgCount = Int

type EndRoutine = IntCodeProgram ()

data ParamMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data ProgramState = ProgramState
  { iPointer :: !Int,
    terminated :: !Bool,
    input :: ![Int],
    output :: [Int],
    memory :: !IntV,
    relativeBase :: !Int,
    result :: !Int
  }

setMemory :: ProgramState -> IntV -> ProgramState
setMemory state memory = state {memory = memory}

instructionRegistry :: [(ArgCount, [InstrCode])]
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

readMemAt :: Int -> IntCodeProgram Int
readMemAt index = allocateMemIfNeeded index >> gets ((! index) . memory)

writeMemAt :: Int -> Int -> IntCodeProgram ()
writeMemAt index value = allocateMemIfNeeded index >> modify' updateMemory
  where
    writeValue vector = write vector index value
    updateMemory state@ProgramState {..} = setMemory state $ modify writeValue memory

valueOf :: Parameter -> IntCodeProgram Int
valueOf (param, mode) =
  case mode of
    Immediate -> return param
    Position -> readMemAt param
    Relative -> gets relativeBase >>= readMemAt . (+ param)

indexOf :: Parameter -> IntCodeProgram Int
indexOf (resultP, mode) =
  case mode of
    Relative -> gets ((+ resultP) . relativeBase)
    _ -> return resultP

yieldState :: Bool -> IntCodeProgram ()
yieldState isTerminated = modify' (\s -> s {terminated = isTerminated})

readInput :: EndRoutine -> Parameter -> IntCodeProgram ()
readInput end parameter =
  gets input
    >>= ( \case
            [] -> yieldState False
            value : rest -> processInput value rest
        )
  where
    processInput value rest = do
      resultP <- indexOf parameter
      writeMemAt resultP value
      modify' (\s -> s {input = rest})
      end

writeOutput :: EndRoutine -> Parameter -> IntCodeProgram ()
writeOutput end parameter = do
  value <- valueOf parameter
  modify' (appendOutput value)
  modify' (\s -> s {result = value})
  end
  where
    appendOutput value state@ProgramState {..} = state {output = value : output}

updateRelativeBase :: EndRoutine -> Parameter -> IntCodeProgram ()
updateRelativeBase end parameter = do
  value <- valueOf parameter
  modify' (updateBase value)
  end
  where
    updateBase value state@ProgramState {..} = state {relativeBase = relativeBase + value}

jumpToPointerIf :: Predicate -> EndRoutine -> ParamT2 -> IntCodeProgram ()
jumpToPointerIf predicate end (param1, param2) = valueOf param1 >>= selectEndRoutine
  where
    setIPointer value state = state {iPointer = value}
    selectEndRoutine value
      | predicate value = valueOf param2 >>= modify' . setIPointer >> nextInstruction
      | otherwise = end

computeValue :: Operator -> EndRoutine -> ParamT3 -> IntCodeProgram ()
computeValue operator end (param1, param2, param3) = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  resultP <- indexOf param3
  writeMemAt resultP (value1 `operator` value2)
  end

runInstruction :: (InstrCode, [Parameter]) -> IntCodeProgram ()
runInstruction codeToParams@(_, params) =
  case codeToParams of
    (3, [param]) -> readInput endRoutine param
    (4, [param]) -> writeOutput endRoutine param
    (9, [param]) -> updateRelativeBase endRoutine param
    (5, [param1, param2]) -> jumpToPointerIf (/= 0) endRoutine (param1, param2)
    (6, [param1, param2]) -> jumpToPointerIf (== 0) endRoutine (param1, param2)
    (1, [param1, param2, param3]) -> computeValue (+) endRoutine (param1, param2, param3)
    (2, [param1, param2, param3]) -> computeValue (*) endRoutine (param1, param2, param3)
    (7, [param1, param2, param3]) -> computeValue (assert (<)) endRoutine (param1, param2, param3)
    (8, [param1, param2, param3]) -> computeValue (assert (==)) endRoutine (param1, param2, param3)
    _ -> illegalState "Invalid instruction configuration: " codeToParams
  where
    endRoutine = moveIPointerBy (length params + 1) >> nextInstruction
    assert cmp lhs rhs
      | lhs `cmp` rhs = 1
      | otherwise = 0

parseParamModes :: [Int] -> [Int] -> IntCodeProgram (InstrCode, [Parameter])
parseParamModes instrTokens args =
  case instrTokens of
    [instrCode] -> codeToParams instrCode zeros
    [instrCode, 0] -> codeToParams instrCode zeros
    [instrCode, 0, pmCode] -> codeToParams instrCode (pmCode : zeros)
    [instrCode, 0, pmCode1, pmCode2] -> codeToParams instrCode (pmCode1 : pmCode2 : zeros)
    [instrCode, 0, pmCode1, pmCode2, pmCode3] -> codeToParams instrCode (pmCode1 : pmCode2 : pmCode3 : zeros)
    _ -> illegalState "Invalid instruction format: " instrTokens
  where
    zeros = replicate (length args) 0
    codeToParams instrCode pmCodes = (instrCode,) . zip args <$> mapM toParamMode pmCodes

processInstruction :: Int -> IntCodeProgram ()
processInstruction instr =
  case fst <$> maybeInstrPair of
    Just argsLen -> get >>= parseParamModes (NE.init tokens) . takeArgs argsLen >>= runInstruction
    Nothing -> illegalState "Unrecognized instruction code: " instr
  where
    tokens@(instrCode :| _) = tokenize instr
    maybeInstrPair = find (elem instrCode . snd) instructionRegistry
    takeArgs argsLen ProgramState {..} = toList $ slice (iPointer + 1) argsLen memory

nextInstruction :: IntCodeProgram ()
nextInstruction = gets memory >>= tryNextInstruction
  where
    tryNextInstruction memory
      | V.null memory = illegalState "Program memory is null: " memory
      | otherwise = gets iPointer >>= readMemAt >>= handleInstruction
    handleInstruction instr
      | instr == 99 = yieldState True
      | otherwise = processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram = execStateT nextInstruction

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
