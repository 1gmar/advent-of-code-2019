{-# LANGUAGE DerivingVia #-}

module Util.IntCodeProgram
  ( ProgramState,
    ProgramResult,
    newProgram,
    programWithInput,
    programWithOutput,
    parseIntCode,
    firstMemoryCell,
    instrPointer,
    inputList,
    isTerminated,
    outputList,
    result,
    runIntCodeProgram,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (StateT, execStateT, gets, modify')
import Data.HashMap.Strict (HashMap, elems, fromList, insert, member, (!))
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import Util.ParseUtils

type IntCodeProgram = StateT ProgramState (Either String)

type ProgramResult = Either String ProgramState

type Parameter = (Int, ParamMode)

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

newtype Index = Index {toInt :: Int}
  deriving (Show)
  deriving (Eq, Enum, Hashable, Num) via Int

type Memory = HashMap Index Int

data ParamMode
  = Position
  | Immediate
  | Relative

data ProgramState = ProgramState
  { iPointer :: !Index,
    terminated :: !Bool,
    input :: ![Int],
    output :: [Int],
    memory :: !Memory,
    relativeBase :: !Int
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

readAt :: Index -> IntCodeProgram Int
readAt index = do
  mem <- gets memory
  unless (index `member` mem) $ 0 `writeAt` index
  gets ((! index) . memory)

writeAt :: Int -> Index -> IntCodeProgram ()
writeAt value index = gets memory >>= modify' . insertValue
  where
    insertValue mem s = s {memory = insert index value mem}

valueOf :: Parameter -> IntCodeProgram Int
valueOf (param, mode) =
  case mode of
    Immediate -> return param
    Position -> readAt $ Index param
    Relative -> gets relativeBase >>= readAt . Index . (+ param)

indexOf :: Parameter -> IntCodeProgram Index
indexOf (resultP, mode) =
  case mode of
    Relative -> gets (Index . (+ resultP) . relativeBase)
    _ -> return $ Index resultP

yieldState :: Bool -> IntCodeProgram ()
yieldState terminated = modify' (\s -> s {terminated = terminated})

readInput :: Parameter -> IntCodeProgram ()
readInput parameter =
  gets input >>= \case
    [] -> yieldState False
    (value : rest) -> do
      index <- indexOf parameter
      value `writeAt` index
      modify' (\s -> s {input = rest})
      nextInstructionWithStep 2

writeOutput :: Parameter -> IntCodeProgram ()
writeOutput parameter = do
  value <- valueOf parameter
  modify' (\s@ProgramState {..} -> s {output = value : output})
  nextInstructionWithStep 2

updateRelativeBase :: Parameter -> IntCodeProgram ()
updateRelativeBase parameter = do
  value <- valueOf parameter
  modify' (\s@ProgramState {..} -> s {relativeBase = relativeBase + value})
  nextInstructionWithStep 2

jumpToPointerIf :: Predicate -> (Parameter, Parameter) -> IntCodeProgram ()
jumpToPointerIf predicate (param1, param2) = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  if predicate value1
    then modify' (\s -> s {iPointer = Index value2}) >> nextInstruction
    else nextInstructionWithStep 3

computeValue :: Operator -> (Parameter, Parameter, Parameter) -> IntCodeProgram ()
computeValue operator (param1, param2, param3) = do
  value <- operator <$> valueOf param1 <*> valueOf param2
  index <- indexOf param3
  value `writeAt` index
  nextInstructionWithStep 4

padPmCodes :: [Int] -> IntCodeProgram [Int]
padPmCodes pmCodes =
  case pmCodes of
    [] -> return zeros
    0 : modes -> return $ modes ++ zeros
    _ -> illegalState "Invalid parameter mode configuration: " pmCodes
  where
    zeros = replicate 4 0

getParams :: [Int] -> Int -> IntCodeProgram [Parameter]
getParams pmCodes nrOfArgs = do
  let takeArgs ProgramState {..} = map (memory !) $ take nrOfArgs [iPointer + 1 ..]
  values <- gets takeArgs
  paramModes <- padPmCodes pmCodes >>= mapM toParamMode
  return $ values `zip` paramModes

processInstruction :: Int -> IntCodeProgram ()
processInstruction instr =
  case tokenize instr of
    (3 :| pmCodes) -> getParams pmCodes 1 >>= mapM_ readInput
    (4 :| pmCodes) -> getParams pmCodes 1 >>= mapM_ writeOutput
    (9 :| pmCodes) -> getParams pmCodes 1 >>= mapM_ updateRelativeBase
    (5 :| pmCodes) -> getParams pmCodes 2 >>= mapM_ (jumpToPointerIf (/= 0)) . toTuple
    (6 :| pmCodes) -> getParams pmCodes 2 >>= mapM_ (jumpToPointerIf (== 0)) . toTuple
    (1 :| pmCodes) -> getParams pmCodes 3 >>= mapM_ (computeValue (+)) . toTuple3
    (2 :| pmCodes) -> getParams pmCodes 3 >>= mapM_ (computeValue (*)) . toTuple3
    (7 :| pmCodes) -> getParams pmCodes 3 >>= mapM_ (computeValue (assert (<))) . toTuple3
    (8 :| pmCodes) -> getParams pmCodes 3 >>= mapM_ (computeValue (assert (==))) . toTuple3
    _ -> illegalState "Unrecognized instruction code: " instr
  where
    toTuple params = zip params (tail params)
    toTuple3 params = zip3 params (tail params) (tail $ tail params)
    assert cmp lhs rhs = if lhs `cmp` rhs then 1 else 0

nextInstructionWithStep :: Index -> IntCodeProgram ()
nextInstructionWithStep iPtrStep = moveIPointer >> nextInstruction
  where
    moveIPointer = modify' (\s@ProgramState {..} -> s {iPointer = iPointer + iPtrStep})

nextInstruction :: IntCodeProgram ()
nextInstruction = do
  instr <- gets iPointer >>= readAt
  if instr == 99 then yieldState True else processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram = execStateT nextInstruction

newProgram :: [Int] -> ProgramState
newProgram memory = ProgramState 0 False [] [] (fromList (zip [0 ..] memory)) 0

firstMemoryCell :: ProgramState -> Maybe Int
firstMemoryCell = listToMaybe . elems . memory

instrPointer :: ProgramState -> Int
instrPointer = toInt . iPointer

inputList :: ProgramState -> [Int]
inputList = input

isTerminated :: ProgramState -> Bool
isTerminated = terminated

outputList :: ProgramState -> [Int]
outputList = reverse . output

programWithInput :: ProgramState -> [Int] -> ProgramState
programWithInput state inputData = state {input = inputData}

programWithOutput :: ProgramState -> [Int] -> ProgramState
programWithOutput state outData = state {output = outData}

result :: ProgramState -> Int
result ProgramState {..} = case output of
  [] -> 0
  r : _ -> r
