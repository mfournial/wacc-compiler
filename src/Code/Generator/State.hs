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
  newVarTable 
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

pushStackVar :: String -> ARM ()
pushStackVar name = state(\junk -> ((), junk{stack = addToStack name (sp junk) (stack junk)})) >> incrementStack

getStackVarOffset :: String -> ARM(Int)
getStackVarOffset name = state (\junk -> (varAddr junk, junk))
  where
    varAddr :: Junk -> Int
    varAddr = fromJust . M.lookup name . head . stack

incrementStack :: ARM ()
incrementStack = state (\junk -> ((), junk{sp = (sp junk) + 1}))

addToStack :: String -> Int -> VarTable -> VarTable
addToStack s addr (m : mps) = M.insert s addr m : mps

newVarTable :: VarTable
newVarTable = [M.empty]