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
import Control.Monad.State (StateT, execStateT, gets, modify')
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
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

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

type IntV = Vector Int

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

tokenize :: Int -> NonEmpty Int
tokenize = unfoldr maybeDiv
  where
    maybeDiv instr
      | instr < 10 = (instr, Nothing)
      | otherwise = Just <$> swap (divMod instr 10)

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

readInput :: [Parameter] -> IntCodeProgram ()
readInput ~[parameter] =
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
      moveIPointerBy 2
      nextInstruction

writeOutput :: [Parameter] -> IntCodeProgram ()
writeOutput ~[parameter] = do
  value <- valueOf parameter
  modify' (appendOutput value)
  modify' (\s -> s {result = value})
  moveIPointerBy 2
  nextInstruction
  where
    appendOutput value state@ProgramState {..} = state {output = value : output}

updateRelativeBase :: [Parameter] -> IntCodeProgram ()
updateRelativeBase ~[parameter] = do
  value <- valueOf parameter
  modify' (updateBase value)
  moveIPointerBy 2
  nextInstruction
  where
    updateBase value state@ProgramState {..} = state {relativeBase = relativeBase + value}

jumpToPointerIf :: Predicate -> [Parameter] -> IntCodeProgram ()
jumpToPointerIf predicate ~[param1, param2] = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  if predicate value1 then modify' (setIPointer value2) else moveIPointerBy 3
  nextInstruction
  where
    setIPointer value state = state {iPointer = value}

computeValue :: Operator -> [Parameter] -> IntCodeProgram ()
computeValue operator ~[param1, param2, param3] = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  resultP <- indexOf param3
  writeMemAt resultP (value1 `operator` value2)
  moveIPointerBy 4
  nextInstruction

getParamModes :: [Int] -> IntCodeProgram [Int]
getParamModes pmCodes =
  case pmCodes of
    [] -> return zeros
    0 : modes -> return $ modes ++ zeros
    _ -> illegalState "Invalid parameter mode configuration: " pmCodes
  where
    zeros = replicate 4 0

runInstruction :: [Int] -> Int -> ([Parameter] -> IntCodeProgram ()) -> IntCodeProgram ()
runInstruction pmCodes argsLen instruction = do
  args <- gets takeArgs
  params <- zip args <$> (getParamModes pmCodes >>= mapM toParamMode)
  instruction params
  where
    takeArgs ProgramState {..} = toList $ slice (iPointer + 1) argsLen memory

processInstruction :: Int -> IntCodeProgram ()
processInstruction instr =
  case tokenize instr of
    (3 :| pmCodes) -> runInstruction pmCodes 1 readInput
    (4 :| pmCodes) -> runInstruction pmCodes 1 writeOutput
    (9 :| pmCodes) -> runInstruction pmCodes 1 updateRelativeBase
    (5 :| pmCodes) -> runInstruction pmCodes 2 $ jumpToPointerIf (/= 0)
    (6 :| pmCodes) -> runInstruction pmCodes 2 $ jumpToPointerIf (== 0)
    (1 :| pmCodes) -> runInstruction pmCodes 3 $ computeValue (+)
    (2 :| pmCodes) -> runInstruction pmCodes 3 $ computeValue (*)
    (7 :| pmCodes) -> runInstruction pmCodes 3 $ computeValue (assert (<))
    (8 :| pmCodes) -> runInstruction pmCodes 3 $ computeValue (assert (==))
    _ -> illegalState "Unrecognized instruction code: " instr
  where
    assert cmp lhs rhs
      | lhs `cmp` rhs = 1
      | otherwise = 0

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
