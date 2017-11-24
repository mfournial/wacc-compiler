{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Code.Generator.State (
  Data,
  Junk(..),
  Instructions,
  ARM,
  getVar',
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
  lookupType,
  newEnv,
  newFunctionEnv,
  closeEnv,
  closeFunctionEnv,
  getStringLiterals,
  getOffsetFromStackPtr,
  addToRuntime,
  storeToRegister,
  updateWithRegister,
  modifyRegister,
  runtimeInstructions,
  push,
  pop,
  referencedPush,
  get
)
where

import Data.Waskell.ADT (NewScope, Type, Expression)
import Data.Waskell.Types (unsfType)

import Data.Bifunctor(first)
import Data.Maybe (fromJust, isNothing)
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
  maybe (put j{strLits = strs |> str} >> return (StringLit (listPosToLabel (length strs)))) (return . StringLit . listPosToLabel) ind

getStringLiterals :: ARM Data
getStringLiterals = fmap strLits get

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (P.length str) |> Ascii str
    labels = fromList $ P.take (length strs) (map listPosToLabel [0..])

listPosToLabel :: Int -> String
listPosToLabel = ("msg_" ++) . show 

newEnv :: NewScope -> ARM ()
newEnv s = modify (\j -> j{stack = M.empty : stack j, scope = s : scope j})

newFunctionEnv :: [String] -- ^ Add the parameters to the function environment
               -> ARM ()
newFunctionEnv params = modify (\j -> j{stack = M.empty : stack j}) >> mapM_ pushVar params

closeEnv :: ARM ()
closeEnv = modify (\j -> j{stack = tail (stack j), scope = tail (scope j)})

closeFunctionEnv :: Int -- ^ Number of parameters of the function
                 -> ARM ()
closeFunctionEnv i = modify (\j -> j{stack = tail (stack j), sp = sp j - 4 * i})

removeFromTable :: String -> Int -> VarTable -> VarTable
removeFromTable s addr (m : mps) = M.insert s addr m : mps

pushVar :: String -> ARM RetLoc
pushVar name = incrementStack >> state (\junk -> (StackPtr (sp junk), junk{stack = addToTable name (sp junk) (stack junk)}))

getOffsetFromStackPtr :: Int -> ARM Int
getOffsetFromStackPtr p = fmap ((subtract p) . sp) get

getVar' :: String -> (Int -> a) -> ARM a
getVar' name f = fmap (f . (varAddr name)) get

addToRuntime :: RCID -> ARM ()
addToRuntime r = modify(\j -> j{runtime = addDependencies r (tryAdd r (runtime j))})
  where
    tryAdd :: Eq a => a -> Seq a -> Seq a
    tryAdd a as = if a `elem` as then as else a <| as
    addDependencies :: RCID -> Seq RCID-> Seq RCID
    addDependencies name names
      | name == ThrowRuntimeErr || name == PrintCharArray = tryAdd PrintStr names
      | name == ThrowDerefRuntimeErr = tryAdd PrintStr names
      | name == Free
          = tryAdd PrintStr $ tryAdd ThrowDerefRuntimeErr names
      | name == Checkdbz || name == NullCheck || name == ArrayCheck || name == ThrowOverflowErr
          = tryAdd PrintStr $ tryAdd ThrowRuntimeErr names
      | otherwise = names

-- Needs to be changed to look into parent scopes
varAddr :: String -> Junk -> Int
varAddr name j = varAddr' name (stack j)
  where
    varAddr' [] _ = error "in VarAddr: Could not find the requested param"
    varAddr' name' (t : ts)
      | isNothing (M.lookup name' t) = varAddr' name' ts
      | otherwise = fromJust $  M.lookup name' t

getStackVar :: String -> ARM RetLoc
getStackVar s = getVar' s StackPtr

incrementStack :: ARM ()
incrementStack = modify (\j -> j{sp = sp j + 4})

decrementStack :: ARM ()
decrementStack = modify (\j -> j{sp = sp j - 4})

getSP :: ARM Int
getSP = fmap sp get 

addToTable :: String -> Int -> VarTable -> VarTable
addToTable s addr (m : mps) = M.insert s addr m : mps

nextLabel :: String -> ARM String
nextLabel s = state (\junk -> ("lab_" ++ show (ref junk) ++ "_" ++ s, junk{ref = ref junk + 1}))

lookupType :: Expression -> ARM Type
lookupType = (`fmap` fmap scope get) . unsfType

newState :: Junk
newState = Junk empty [] [] 0 0 empty

storeToRegister :: Reg -> RetLoc -> ARM Instructions
storeToRegister r (PRL k)     = return $ storeToRegisterPure r k
storeToRegister r l           = modifyRegister     storeToRegister' r l

updateWithRegister :: Reg -> RetLoc -> ARM Instructions
updateWithRegister r (PRL k)     = return $ updateWithRegisterPure r k
updateWithRegister r l           = modifyRegister updateWithRegister' r l

modifyRegister :: RegMod -> Reg -> RetLoc -> ARM Instructions
modifyRegister f r (StackPtr i) = do
  off <- getOffsetFromStackPtr i
  return $ f r (OffReg StackPointer (Int off) False)   

modifyRegister f r (PRL k) = return $ modifyRegisterPure f r k

runtimeInstructions :: ARM (Seq RCID)
runtimeInstructions = fmap runtime get

push :: [Reg] -> ARM (Instr, [RetLoc])
push rs = do
  locs <- mapM (\r -> incrementStack>> getSP >>= return . StackPtr) rs
  return (PUSH rs, locs)

pop :: [Reg] -> ARM Instr
pop = fmap POP . (mapM (\r -> decrementStack >> return r))

referencedPush :: [Reg] -> [String] -> ARM (Instr, [RetLoc])
referencedPush = ((.).(.)) ((fmap (first PUSH . P.unzip)) . (mapM (\ (r,n) -> pushVar n >>= \k -> return (r,k)))) P.zip
