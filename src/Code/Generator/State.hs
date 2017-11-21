{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Code.Generator.State (
  Data,
  Junk(..),
  Instructions,
  ARM,
  runState,
  execState,
  dataSection,
  newStringLiteral,
  addToHeap,
  getFromHeap,
  pushVar,
  incrementStack,
  decrementStack,
  newState,
  nextLabel,
  newEnv,
  closeEnv,
  getStringLiterals,
  getOffsetFromStackPtr,
  addToRuntime,
  getVar,
  getSP
)
where

import Data.Char(intToDigit)
import Data.Maybe(fromJust, isJust)
import Data.Sequence
import Data.Sequence.Util
import Control.Monad.State.Lazy
import Prelude hiding (concat, length, zipWith, null, lookup)
import qualified Prelude as P

import qualified Data.HashMap.Strict as M

import Code.Instructions
import Code.Generator.RetLoc.Internal
import Code.Generator.Runtime.Internal(RCID)
--import Data.Waskell.ADT 

type ARM = State Junk

newStringLiteral :: String -> ARM PureRetLoc
newStringLiteral str = do
  j <- get
  let strs = strLits j
  let ind = elemIndexL str strs
  maybe (put j{strLits = strs |> str} >> return (StringLit (listPosToLabel (length strs)))) (\i -> return (StringLit (listPosToLabel i))) ind

getStringLiterals :: ARM Data
getStringLiterals = state (\junk -> (strLits junk, junk))

data Junk = Junk {
  strLits :: Data,
  stack :: VarTable,
  heap :: VarTable,
  sp :: Int,
  ref :: Int,
  runtime :: Seq RCID
} 

type VarTable = [M.HashMap String Int]
type Data = Seq String

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (size str 0) |> Ascii str
    labels = fromList $ P.take (length strs) (map listPosToLabel [0..])

listPosToLabel :: Int -> String
listPosToLabel = ("msg_" ++) . pure . intToDigit

-- | Can't import length from prelude... because of conflicts
size :: String -> Int-> Int
size [] i = i
size (c : cs) i = size cs (i + 1)

newEnv :: ARM ()
newEnv = state (\junk -> ((), junk{stack = M.empty : stack junk, heap = M.empty : heap junk}))

closeEnv :: ARM ()
closeEnv = state (\junk -> ((), junk{stack = tail (stack junk), heap = tail (heap junk)}))

removeFromTable :: String -> Int -> VarTable -> VarTable
removeFromTable s addr (m : mps) = M.insert s addr m : mps

pushVar :: String -> ARM RetLoc
pushVar name = state (\junk -> ((), junk{stack = addToTable name (sp junk) (stack junk)})) >> incrementStack >> fmap StackPtr getSP

addToHeap :: String -> Int -> ARM PureRetLoc
addToHeap name address = state (\junk -> (HeapAddr address, junk{heap = addToTable name address (heap junk)}))

getOffsetFromStackPtr :: Int -> ARM Int
getOffsetFromStackPtr p = state (\junk -> (sp junk - p, junk))

getFromHeap :: String -> ARM (Maybe PureRetLoc)
getFromHeap name = state (\junk -> (fmap HeapAddr $ varAddr name heap junk, junk))

getStackVarPtr :: String -> ARM (Maybe RetLoc)
getStackVarPtr name = state (\junk -> (fmap StackPtr $ varAddr name stack junk, junk))

addToRuntime :: RCID -> ARM ()
addToRuntime r = state (\junk -> ((), junk{runtime = tryAdd r ((runtime junk))}))
  where
    tryAdd :: Eq a => a -> Seq a -> Seq a
    tryAdd a as = if a `elem` as then as else a <| as 

-- Needs to be changed to look into parent scopes
varAddr :: String -> (Junk -> VarTable) -> Junk -> Maybe Int
varAddr name = ((M.lookup name . head) .)

getVar :: String -> ARM RetLoc
getVar s = do
  h <- getFromHeap s
  fmap fromJust $ if isJust h then return (fmap PRL h) else getStackVarPtr s

incrementStack :: ARM ()
incrementStack = state (\junk -> ((), junk{sp = (sp junk) + 1}))

decrementStack :: ARM ()
decrementStack = state (\junk -> ((), junk{sp = (sp junk) - 1}))

getSP :: ARM Int
getSP = fmap sp get 

addToTable :: String -> Int -> VarTable -> VarTable
addToTable s addr (m : mps) = M.insert s addr m : mps

nextLabel :: String -> ARM (String)
nextLabel s = state (\junk -> (("lab_" ++ [intToDigit (ref junk)] ++ "_" ++ s), junk{ref = ref junk + 1}))

newState :: Junk
newState = Junk empty [M.empty] [M.empty] 0 0 empty
