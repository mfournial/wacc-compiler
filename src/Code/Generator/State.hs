{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Code.Generator.State (
  Data,
  Junk(..),
  Instructions,
  ARM,
  runState,
  execState,
  getStackVar,
  dataSection,
  newStringLiteral,
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
  getSP,
  storeToRegister,
  updateWithRegister,
  modifyRegister,
  runtimeInstructions
)
where

import Data.Char(intToDigit)
import Data.Maybe(fromJust)
import Data.Sequence
import Data.Sequence.Util
import Control.Monad.State.Lazy
import Prelude hiding (concat, length, zipWith, null, lookup)
import qualified Prelude as P

import qualified Data.HashMap.Strict as M

import Code.Instructions
import Code.Generator.ARM
--import Data.Waskell.ADT 

newStringLiteral :: String -> ARM PureRetLoc
newStringLiteral str = do
  j <- get
  let strs = strLits j
  let ind = elemIndexL str strs
  maybe (put j{strLits = strs |> str} >> return (StringLit (listPosToLabel (length strs)))) (\i -> return (StringLit (listPosToLabel i))) ind

getStringLiterals :: ARM Data
getStringLiterals = state (\junk -> (strLits junk, junk))

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (P.length str) |> Ascii str
    labels = fromList $ P.take (length strs) (map listPosToLabel [0..])

listPosToLabel :: Int -> String
listPosToLabel = ("msg_" ++) . pure . intToDigit

newEnv :: ARM ()
newEnv = state (\junk -> ((), junk{stack = M.empty : stack junk, heap = M.empty : heap junk}))

closeEnv :: ARM ()
closeEnv = state (\junk -> ((), junk{stack = tail (stack junk), heap = tail (heap junk)}))

removeFromTable :: String -> Int -> VarTable -> VarTable
removeFromTable s addr (m : mps) = M.insert s addr m : mps

pushVar :: String -> ARM RetLoc
pushVar name = state (\junk -> (StackPtr (sp junk), junk{stack = addToTable name (sp junk) (stack junk)})) >>= \s -> incrementStack >> return s

getOffsetFromStackPtr :: Int -> ARM Int
getOffsetFromStackPtr p = state (\junk -> (sp junk - p, junk))

getVar' :: String -> (Int -> RetLoc) -> ARM RetLoc
getVar' name f = state (\junk -> (f $ varAddr name junk, junk))

addToRuntime :: RCID -> ARM ()
addToRuntime r = state (\junk -> ((), junk{runtime = addDependencies r (tryAdd r (runtime junk))}))
  where
    tryAdd :: Eq a => a -> Seq a -> Seq a
    tryAdd a as = if a `elem` as then as else a <| as
    addDependencies :: RCID -> Seq RCID-> Seq RCID
    addDependencies name names
      | name == ThrowRuntimeErr = tryAdd PrintStr names
      | name == Checkdbz || name == ArrayCheck || name == ThrowOverflowErr
          = tryAdd PrintStr $ tryAdd ThrowRuntimeErr names
      | otherwise = names

-- Needs to be changed to look into parent scopes
varAddr :: String -> Junk -> Int
varAddr name j = fromJust (M.lookup name (head (stack j)))

getStackVar :: String -> ARM RetLoc
getStackVar s = getVar' s StackPtr

incrementStack :: ARM ()
incrementStack = state (\junk -> ((), junk{sp = sp junk + 4}))

decrementStack :: ARM ()
decrementStack = state (\junk -> ((), junk{sp = sp junk - 4}))

getSP :: ARM Int
getSP = fmap sp get 

addToTable :: String -> Int -> VarTable -> VarTable
addToTable s addr (m : mps) = M.insert s addr m : mps

nextLabel :: String -> ARM String
nextLabel s = state (\junk -> ("lab_" ++ [intToDigit (ref junk)] ++ "_" ++ s, junk{ref = ref junk + 1}))

newState :: Junk
newState = Junk empty [M.empty] [M.empty] 0 0 empty

storeToRegister :: Reg -> RetLoc -> ARM Instructions
storeToRegister r (PRL k)     = return $ storeToRegisterPure r k
storeToRegister r l           = modifyRegister     storeToRegister' r l

updateWithRegister :: Reg -> RetLoc -> ARM Instructions
updateWithRegister r (PRL k)     = return $ updateWithRegisterPure r k
updateWithRegister r l           = modifyRegister updateWithRegister' r l

modifyRegister :: RegMod -> Reg -> RetLoc -> ARM Instructions
modifyRegister f r (StackPtr i) = do
  off <- getOffsetFromStackPtr i
  return $ f r (OffReg StackPointer (offsetToARMOffset off) False)   

modifyRegister f r (PRL k) = return $ modifyRegisterPure f r k

runtimeInstructions :: ARM (Seq RCID)
runtimeInstructions = state (\junk -> (runtime junk, junk))
