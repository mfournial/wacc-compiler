module Code.Generator.ARM where

import Control.Monad.State.Lazy(State)
import Data.Sequence
import Data.Waskell.ADT(NewScope)
import qualified Data.HashMap.Strict as M

import Code.Instructions

-- | State for ARM generation
type ARM = State ARMData

-- | State fields
data ARMData = ARMData {
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
data RCID = PrintStr
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
data Location = PRL PureLocation | StackPtr Int 
  deriving (Show)

-- | Different types of Addresses (Expression or pre-index offset)
data PureLocation = StringLit String
                  | RegLoc Reg
                  | RegLocOffset Reg Int
                  | Register Reg
                  | ImmChar Char
                  | ImmInt Int
  deriving (Show)

-- | Function type that returns ARM instructions that use Addresses
type RegMod = Reg -> Address -> Instructions

-- | store to register from an immediate val or register
storePure :: Reg -> PureLocation -> Instructions
storePure r (ImmInt i)    = singleton (LDR AL W r (Const i))
storePure r (ImmChar c)   = singleton (MOV AL F r (ImmOpCh c))
storePure r (Register r') = storeRegs r r'
storePure r k             = modifyRegisterPure store' r k

-- | store the value from register into a different register or memory
-- | throw an error if trying to update an immediate value
updatePure :: Reg -> PureLocation -> Instructions
updatePure r (ImmInt i)    = error "Attempting to update immediate value"
updatePure r (ImmChar c)   = error "Attempting to update immediate value"
updatePure r (Register r') = storeRegs r' r
updatePure r k             = modifyRegisterPure update' r k

-- | helper to convert PureLocation into Addresses 
modifyRegisterPure :: RegMod -> Reg -> PureLocation -> Instructions
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
store' :: Reg -> Address -> Instructions
store' r a = singleton (LDR AL W r a)

-- | store from register with immediate offset or pre-indexed offset
update' :: Reg -> Address -> Instructions
update' r a = singleton (STR AL W r a)
  

