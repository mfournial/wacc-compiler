{-|

== State for ARM code generation

State keeps tracks of the current stack position and keeps all informations that
cannot be computed immediately by the other functions.
It is mainly use to calculate stack offsets throughout the program and store
string literal

Group 26 -- Waskell
Module      : main
Maintainer  : mmf115@ic.ac.uk
Portability : POSIX

This module will return a state containing the possible wanted informations.
Functions may perform side effect inside the state that could impact other
parts of the program.

-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Code.Generator.State
  ( Data
  , Junk(..)
  , Instructions
  , ARM
  , getVar
  , runState
  , execState
  , getStackVar
  , dataSection
  , newStringLiteral
  , pushVar
  , incrementStack
  , decrementStack
  , newState
  , nextLabel
  , lookupType
  , newEnv
  , closeEnv
  , getOffsetFromStackPtr
  , addToRuntime
  , storeToRegister
  , updateWithRegister
  , modifyRegister
  , runtimeInstructions
  , push
  , pop
  , referencedPush
  , get
  ) where

import Code.Generator.ARM
import Code.Instructions
import Data.Waskell.ADT (Expression, NewScope, Type)
import Data.Waskell.Types (unsfType)

import Control.Monad.State.Lazy
import Data.Bifunctor (first)
import Data.Maybe (fromJust, isNothing)
import qualified Data.HashMap.Strict as M
import Data.Sequence
import Data.Sequence.Util
import Prelude hiding (concat, length, lookup, null, zipWith)
import qualified Prelude as P


-- | newStringLiteral adds the given string to the state to add in the data
-- section on top of the file at the end of the compilation
newStringLiteral :: String -> ARM PureRetLoc
newStringLiteral str = do
  j <- get
  let strs = strLits j
  let ind = elemIndexL str strs
  maybe
    (put j {strLits = strs |> str} >>
     return (StringLit (listPosToLabel (length strs))))
    (return . StringLit . listPosToLabel)
    ind

-- | dataSection is meant to be called at the end of the compilation once state
-- has returned. It computes all the required data to return the instructions
-- to build the data section corresponding to the final state of the program
-- Returns empty if there is no need for a data section (strs in empty)
dataSection :: Data -- ^ The complete list of literals at the end of compilation
            -> Instructions -- ^ The instructions to print data section
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str =
      empty |> Define label |> Word (P.length str) |> Ascii str
    labels = fromList $ P.take (length strs) (map (listPosToLabel) [0 ..])

-- | listPosToLabel computes the appropriate string label given the current
-- count of labels. the data section and the newStringLiteral funciton should
-- both use it for matching labels
listPosToLabel :: Int -- ^ the current count of labels
               -> String -- ^ the label for the string
listPosToLabel = ("msg_" ++) . show

-- | Creates a new local stack environment and associates it with a NewScope
-- that contains the type information and identifiers of the local variables
newEnv :: NewScope -- ^ the scope containing the variable Identifiers and types
       -> ARM ()
newEnv s = modify (\j -> j {stack = M.empty : stack j, scope = s : scope j})

-- | @ id = newEnv >> closeEnv @
-- Undo all actions made by new Env, return to previous stack state
closeEnv :: ARM ()
closeEnv = modify (\j -> j {stack = tail (stack j), scope = tail (scope j)})

-- | pushVar updates our local stack. It gives a local stack address to the
-- given identifier to be able to calculate its offset later
pushVar :: String -- ^ The identifier of the var to push
        -> ARM RetLoc
pushVar name =
  incrementStack >>
  state
    (\junk ->
       ( StackPtr (sp junk)
       , junk {stack = addToTable name (sp junk) (stack junk)}))
  where
    addToTable :: String -- ^ The identifier of the elem to add to the table
               -> Int -- ^ The address of the identifier
               -> VarTable -- ^ The table to add it to
               -> VarTable -- ^ The resulting table
    addToTable s addr (m:mps) = M.insert s addr m : mps

-- | pushes the registers and updates the virtual stack
push :: [Reg] -- ^ the list of regs to push
     -> ARM (Instr, [RetLoc]) -- ^ the instructions to push the regs with local addresses
push rs = do
  locs <- mapM (\r -> incrementStack >> (fmap sp get) >>= return . StackPtr) rs
  return (PUSH rs, locs)

-- | decreases our virtual stack by the number of given registers
pop :: [Reg] -> ARM Instr
pop = fmap POP . (mapM (\r -> decrementStack >> return r))

-- | Similar to push but gives a name to the variable to push on the stack for
-- later retrieval
referencedPush :: [Reg] -- ^ registers to push 
               -> [String] -- ^ the identifiers of the variables
               -> ARM (Instr, [RetLoc])
referencedPush =
  ((.) . (.))
    ((fmap (first PUSH . P.unzip)) .
     (mapM (\(r, n) -> pushVar n >>= \k -> return (r, k))))
    P.zip

-- | Calculates the offset given the virtual stack address of the var
getOffsetFromStackPtr :: Int -- ^ the virtual stack address of the var
                      -> ARM Int -- ^ the offset according to current sp
getOffsetFromStackPtr p = fmap ((subtract p) . sp) get

-- | Specialised function called by array element to get variable address
getVar :: String -- ^ the identifier of the variable
        -> ARM Int -- ^ the virtual address of the variable
getVar name = fmap (varAddr name) get

-- | varAddr lookup the address of the given variable in the virtual stack
varAddr :: String -> Junk -> Int
varAddr name j = varAddr' name (stack j)
  where
    varAddr' [] _ = error "in VarAddr: Could not find the requested param"
    varAddr' name' (t:ts)
      | isNothing (M.lookup name' t) = varAddr' name' ts
      | otherwise = fromJust $ M.lookup name' t

-- | getStackVar returns the location of the given identifier
getStackVar :: String -- ^ the identifier of the variable
            -> ARM RetLoc -- ^ The location of the variable
getStackVar s = fmap StackPtr $ getVar s 

-- | addToRuntime manages the runtime state environment. It also adds the
-- dependencies the functions depends on. The tryAdd function ensure uniqueness
-- of the elements in the runtime.
-- tryAdd adds unique element in the sequence
--  @ tryAdd b [a, b, c] = id @
--  @ tryAdd d [a, b, c] = [a, b, c, d] @
-- addDependencies add the correct dependencies
addToRuntime :: RCID -- ^ the Runctime component identifier to add
             -> ARM ()
addToRuntime r =
  modify (\j -> j {runtime = addDependencies r (tryAdd r (runtime j))})
  where
    tryAdd :: Eq a => a -- ^ the element to add if not in list
           -> Seq a -- ^ the sequence to add it to
           -> Seq a -- ^ the resulting sequence
    tryAdd a as =
      if a `elem` as
        then as
        else a <| as
    addDependencies :: RCID -> Seq RCID -> Seq RCID
    addDependencies name names
      | elem name printDepen = tryAdd PrintStr names
      | name == Free = tryAdd PrintStr $ tryAdd ThrowDerefRuntimeErr names
      | elem name runErrDepen = tryAdd PrintStr $ tryAdd ThrowRuntimeErr names
      | otherwise = names
    printDepen = [ThrowRuntimeErr, PrintCharArray, ThrowDerefRuntimeErr]
    runErrDepen = [Checkdbz, NullCheck, ArrayCheck, ThrowOverflowErr]

-- | Grows the stack by a word
incrementStack :: ARM ()
incrementStack = modify (\j -> j {sp = sp j + 4})

-- | Shrinks stack by a word
decrementStack :: ARM ()
decrementStack = modify (\j -> j {sp = sp j - 4})

-- | returns an original label prefixed with lab and terminating with the 
-- current label count. Updates the label count as a side-effect
nextLabel :: String -> ARM String
nextLabel s =
  state
    (\junk -> ("lab_" ++ show (ref junk) ++ "_" ++ s, junk {ref = ref junk + 1}))

-- | returns the type of an expression
lookupType :: Expression -- ^ the expression we want to know the type of
           -> ARM Type -- ^ The type of the expression
lookupType = (`fmap` fmap scope get) . unsfType

-- | helper that initialises the state
newState :: Junk
newState = Junk empty [] [] 0 0 empty

-- | Stores the storable into the given register
storeToRegister :: Reg -- ^ the register to store the var to
                -> RetLoc -- ^ The storable to be stored
                -> ARM Instructions -- ^ the instructions to store the value
storeToRegister r (PRL k) = return $ storeToRegisterPure r k
storeToRegister r l       = modifyRegister storeToRegister' r l

-- | Stores the value in the register into the given location
updateWithRegister :: Reg -- ^ the register to store
                   -> RetLoc -- ^ where to store the register
                   -> ARM Instructions -- ^ the instructions to store the reg
updateWithRegister r (PRL k) = return $ updateWithRegisterPure r k
updateWithRegister r l       = modifyRegister updateWithRegister' r l

-- | Changes register values
modifyRegister :: RegMod -- ^ Function to modify the register
               -> Reg -- ^ First register operand
               -> RetLoc -- ^ Storable operand
               -> ARM Instructions -- ^ the instructions to execute RegMod
modifyRegister f r (StackPtr i) = do
  off <- getOffsetFromStackPtr i
  return $ f r (OffReg StackPointer (Int off) False)   
modifyRegister f r (PRL k) = return $ modifyRegisterPure f r k

-- | Returns the runtime instructions at the end of the compilation
runtimeInstructions :: ARM (Seq RCID)
runtimeInstructions = fmap runtime get
