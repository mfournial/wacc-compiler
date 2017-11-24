module Code.Generator.ARM where

import Control.Monad.State.Lazy(State)
import Data.Sequence
import Data.Waskell.ADT(NewScope)
import qualified Data.HashMap.Strict as M

import Code.Instructions

-- | State for ARM generation
type ARM = State Junk
 
-- | State fields
data Junk = Junk {
  strLits :: Data,
  stack :: VarTable,
  scope :: [NewScope],
  sp :: Int,
  ref :: Int,
  runtime :: Seq RCID
} 

-- | A Hashmap of Variables and Addresses 
type VarTable = [M.HashMap String Int]

-- | Data represented as a sequence of Strings
type Data = Seq String

-- | RuntimeComponent type 
data RuntimeComponent = RC RCID Instructions

instance Eq RuntimeComponent where
  (==) (RC rid _) (RC rid' _) = rid == rid'

-- | Runtime Component Identifiers
data RCID = 
      PrintStr
    | PrintInt
    | PrintChar
    | PrintCharArray
    | PrintBool
    | ReadInt
    | ReadChar
    | ThrowRuntimeErr
    | ThrowDerefRuntimeErr
    | ThrowOverflowErr
    | Free
    | PrintRef
    | ArrayCheck
    | NullCheck
    | Checkdbz
  deriving (Eq)

-- | Storeable that can be of Address type or a StackPtr
data RetLoc = PRL PureRetLoc | StackPtr Int 
  deriving (Show)

-- | Different types of Addresses (Expression or pre-index offset)
data PureRetLoc = 
      StringLit String
    | RegLoc Reg
    | RegLocOffset Reg Int
    | Register Reg
    | ImmChar Char
    | ImmInt Int
  deriving (Show)

-- | Function type that returns ARM instructions that use Addresses
type RegMod = Reg -> Address -> Instructions

-- | store to register from an immediate val or register
storeToRegisterPure :: Reg -> PureRetLoc -> Instructions
storeToRegisterPure r (ImmInt i)    = singleton (LDR AL W r (Const i))
storeToRegisterPure r (ImmChar c)   = singleton (MOV AL F r (ImmOpCh c))
storeToRegisterPure r (Register r') = storeRegs r r'
storeToRegisterPure r k             = modifyRegisterPure storeToRegister' r k

-- | store the value from register into a different register or memory
-- | throw an error if trying to update an immediate value
updateWithRegisterPure :: Reg -> PureRetLoc -> Instructions
updateWithRegisterPure r (ImmInt i)    = error "Attempting to update immediate value"
updateWithRegisterPure r (ImmChar c)   = error "Attempting to update immediate value"
updateWithRegisterPure r (Register r') = storeRegs r' r
updateWithRegisterPure r k             = modifyRegisterPure updateWithRegister' r k

-- | helper to convert PureRetLoc into Addresses 
modifyRegisterPure :: RegMod -> Reg -> PureRetLoc -> Instructions
modifyRegisterPure f r (StringLit str) = f r (Label str)
modifyRegisterPure f r' (RegLocOffset r o) = f r' (OffReg r (Int o) False)
modifyRegisterPure f r' (RegLoc r) = f r' (OffReg r (Int 0) False)
modifyRegisterPure _ _ _ = error "modify cannot work on immediate values"

-- | moves a value from register r' to register r
storeRegs :: Reg -> Reg -> Instructions
storeRegs r r'
  | r == r' = empty
  | otherwise = singleton (MOV AL F r (ShiftReg r' NSH))

-- | loads to register with immediate offset or pre-indexed offset
storeToRegister' :: Reg -> Address -> Instructions
storeToRegister' r a = singleton (LDR AL W r a)

-- | store from register with immediate offset or pre-indexed offset
updateWithRegister' :: Reg -> Address -> Instructions
updateWithRegister' r a = singleton (STR AL W r a)
  

