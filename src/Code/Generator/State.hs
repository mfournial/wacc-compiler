{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Code.Generator.State (
  Data,
  Junk(..),
  Instructions,
  ARM,
  concat,
  runState,
  execState,
  dataSection,
  newStringLiteral,
  addToHeap,
  getFromHeap,
  push,
  pop,
  newState,
  newEnv,
  closeEnv
)
where

import Data.Char(intToDigit)
import Data.Maybe(fromJust)
import Data.Sequence
import Control.Monad.State.Lazy
import Prelude hiding (concat, length, zipWith, null)

import qualified Data.HashMap.Strict as M

import Code.Instructions
--import Data.Waskell.ADT 

type Instructions = Seq Instr
type ARM = State Junk

newStringLiteral :: String -> ARM ()
newStringLiteral str = state (\junk -> ((), junk{strLits = strLits junk |> str}))

data Junk = Junk {
  strLits :: Data,
  stack :: VarTable,
  heap :: VarTable,
  sp :: Int
}

type VarTable = [M.HashMap String Int]
type Data = Seq String

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (size str 0) |> Ascii str
    labels             = fromList (map (("msg_" ++) . (pure .intToDigit)) [0..])

concat :: Seq (Seq a) -> Seq a
concat = foldl (><) empty

-- | Can't import length from prelude... because of conflicts
size :: String -> Int-> Int
size [] i = i
size (c : cs) i = size cs (i + 1)

newEnv :: ARM ()
newEnv = state (\junk -> ((), junk{stack = M.empty : stack junk, heap = M.empty : heap junk}))

pop :: ARM ()
pop = decrementStack

closeEnv :: ARM ()
closeEnv = state (\junk -> ((), junk{stack = tail (stack junk), heap = tail (heap junk)}))

removeFromTable :: String -> Int -> VarTable -> VarTable
removeFromTable s addr (m : mps) = M.insert s addr m : mps

push :: String -> ARM ()
push name = state (\junk -> ((), junk{stack = addToTable name (sp junk) (stack junk)})) >> incrementStack

addToHeap :: String -> Int -> ARM ()
addToHeap name address = state (\junk -> ((), junk{heap = addToTable name address (heap junk)}))

getFromHeap :: String -> ARM(Int)
getFromHeap name = state (\junk -> (varAddr name heap junk, junk))

getStackVarOffset :: String -> ARM(Int)
getStackVarOffset name = state (\junk -> (sp junk - varAddr name stack junk, junk))


-- Needs to be changed to look into parent scopes
varAddr :: String -> (Junk -> VarTable) -> Junk -> Int
varAddr name = ((fromJust . M.lookup name . head) .)

incrementStack :: ARM ()
incrementStack = state (\junk -> ((), junk{sp = (sp junk) + 4}))

decrementStack :: ARM ()
decrementStack = state (\junk -> ((), junk{sp = (sp junk) - 4}))

addToTable :: String -> Int -> VarTable -> VarTable
addToTable s addr (m : mps) = M.insert s addr m : mps

newState :: Junk
newState = Junk empty [M.empty] [M.empty] 0