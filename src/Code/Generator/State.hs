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
  addToRuntime,
  getOffsetFromStackPtr
)
where

import Data.Char(intToDigit)
import Data.Maybe(fromJust, isJust)
import Data.Sequence
import Data.Sequence.Util
import Control.Monad.State.Lazy
import Prelude hiding (concat, length, zipWith, null, take)
import qualified Prelude as P

import qualified Data.HashMap.Strict as M

import Code.Instructions
import Code.Generator.RetLoc.Internal
import Code.Generator.Runtime.Internal(RuntimeComponent)
--import Data.Waskell.ADT 

type ARM = State Junk

newStringLiteral :: String -> ARM PureRetLoc
newStringLiteral str = state (\junk -> (StringLit (listPosToLabel (length (strLits junk))), junk{strLits = strLits junk |> str}))

getStringLiterals :: ARM Data
getStringLiterals = state (\junk -> (strLits junk, junk))

data Junk = Junk {
  strLits :: Data,
  stack :: VarTable,
  heap :: VarTable,
  sp :: Int,
  ref :: Int,
  runtime :: [RuntimeComponent]
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

pushVar :: String -> ARM ()
pushVar name = state (\junk -> ((), junk{stack = addToTable name (sp junk) (stack junk)})) >> incrementStack

addToHeap :: String -> Int -> ARM ()
addToHeap name address = state (\junk -> ((), junk{heap = addToTable name address (heap junk)}))

getOffsetFromStackPtr :: Int -> ARM Int
getOffsetFromStackPtr p = state (\junk -> (sp junk - p, junk))

getFromHeap :: String -> ARM (Maybe PureRetLoc)
getFromHeap name = state (\junk -> (fmap HeapAddr $ varAddr name heap junk, junk))

getStackVarPtr :: String -> ARM (Maybe RetLoc)
getStackVarPtr name = state (\junk -> (fmap StackPtr $ varAddr name stack junk, junk))

addToRuntime :: RuntimeComponent -> ARM ()
addToRuntime r = state (\junk -> ((), junk{runtime = tryAdd r (runtime junk)}))
  where
    tryAdd :: RuntimeComponent -> [RuntimeComponent] -> [RuntimeComponent]
    tryAdd rc rs = if rc `elem` rs then rs else rc : rs

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

addToTable :: String -> Int -> VarTable -> VarTable
addToTable s addr (m : mps) = M.insert s addr m : mps

nextLabel :: String -> ARM (String)
nextLabel s = state (\junk -> (("lab_" ++ [intToDigit (ref junk)] ++ s), junk{ref = ref junk + 1}))

newState :: Junk
newState = Junk empty [M.empty] [M.empty] 0 0 []
