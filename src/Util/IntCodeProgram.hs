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

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (StateT, execStateT, gets, modify')
import Data.HashMap.Strict (HashMap, elems, fromList, insert, member, (!))
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
import Data.Tuple (swap)
import Util.ParseUtils

type IntCodeProgram = StateT ProgramState (Either String)

type ProgramResult = Either String ProgramState

type Parameter = (Int, ParamMode)

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

type Memory = HashMap Int Int

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
    memory :: !Memory,
    relativeBase :: !Int,
    result :: !Int
  }

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

readMemAt :: Int -> IntCodeProgram Int
readMemAt index = do
  mem <- gets memory
  unless (index `member` mem) $ writeMemAt index 0
  gets ((! index) . memory)

writeMemAt :: Int -> Int -> IntCodeProgram ()
writeMemAt index value = gets memory >>= modify' . insertValue
  where
    insertValue mem = \s -> s {memory = insert index value mem}

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
            params -> processInput params
        )
  where
    processInput ~(value : rest) = do
      resultP <- indexOf parameter
      writeMemAt resultP value
      modify' (\s -> s {input = rest})
      nextInstructionWithStep 2

writeOutput :: [Parameter] -> IntCodeProgram ()
writeOutput ~[parameter] = do
  value <- valueOf parameter
  modify' (\s@ProgramState {..} -> s {output = value : output})
  modify' (\s -> s {result = value})
  nextInstructionWithStep 2

updateRelativeBase :: [Parameter] -> IntCodeProgram ()
updateRelativeBase ~[parameter] = do
  value <- valueOf parameter
  modify' (\s@ProgramState {..} -> s {relativeBase = relativeBase + value})
  nextInstructionWithStep 2

jumpToPointerIf :: Predicate -> [Parameter] -> IntCodeProgram ()
jumpToPointerIf predicate ~[param1, param2] = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  if predicate value1
    then modify' (\s -> s {iPointer = value2}) >> nextInstruction
    else nextInstructionWithStep 3

computeValue :: Operator -> [Parameter] -> IntCodeProgram ()
computeValue operator ~[param1, param2, param3] = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  resultP <- indexOf param3
  writeMemAt resultP (value1 `operator` value2)
  nextInstructionWithStep 4

padPmCodes :: [Int] -> IntCodeProgram [Int]
padPmCodes pmCodes =
  case pmCodes of
    [] -> return zeros
    0 : modes -> return $ modes ++ zeros
    _ -> illegalState "Invalid parameter mode configuration: " pmCodes
  where
    zeros = replicate 4 0

runInstruction :: [Int] -> Int -> ([Parameter] -> IntCodeProgram ()) -> IntCodeProgram ()
runInstruction pmCodes argsLen instruction = do
  args <- gets takeArgs
  params <- zip args <$> (padPmCodes pmCodes >>= mapM toParamMode)
  instruction params
  where
    takeArgs ProgramState {..} = map (memory !) $ take argsLen [iPointer + 1 ..]

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

nextInstructionWithStep :: Int -> IntCodeProgram ()
nextInstructionWithStep iPtrStep = moveIPointer >> nextInstruction
  where
    moveIPointer = modify' (\s@ProgramState {..} -> s {iPointer = iPointer + iPtrStep})

nextInstruction :: IntCodeProgram ()
nextInstruction = do
  instr <- gets iPointer >>= readMemAt
  if instr == 99 then yieldState True else processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram = execStateT nextInstruction

newProgram :: [Int] -> ProgramState
newProgram memory = ProgramState 0 False [] [] (fromList (zip [0 ..] memory)) 0 0

programMemory :: ProgramState -> [Int]
programMemory = elems . memory

outputList :: ProgramState -> [Int]
outputList = reverse . output

programWithInput :: ProgramState -> [Int] -> ProgramState
programWithInput state inputData = state {input = inputData}

programWithOutput :: ProgramState -> [Int] -> ProgramState
programWithOutput state outData = state {output = outData}
