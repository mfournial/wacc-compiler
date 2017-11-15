{-# LANGUAGE FlexibleInstances #-}
module Code.Instructions where

class PrintARM a where
  printARM :: a -> String

data Instr = Section String
           | Word Int
           | Ascii String
           | Define String
           | Global
           | MOV Condition Set Reg Op2
           | MVN Condition Set Reg Op2
           | B   Condition String
           | BL  Condition String
           | CMP Condition Reg Op2
           | CMN Condition Reg Op2
           | TEQ Condition Reg Op2
           | TST Condition Reg Op2
           | AND Condition Set Reg Reg Op2
           | EOR Condition Set Reg Reg Op2
           | BIC Condition Set Reg Reg Op2
           | ORR Condition Set Reg Reg Op2
           | SUB Condition Set Reg Reg Op2
           | RSB Condition Set Reg Reg Op2
           | ADD Condition Set Reg Reg Op2
           | MUL Condition Set Reg Reg Reg
           | MLA Condition Set Reg Reg Reg
           | LDR Condition Mem Reg Address
           | STR Condition Mem Reg Address
           | PUSH Condition [Reg]
           | POP Condition [Reg]
           deriving (Eq)

data Set = T
         | F
         deriving (Eq)

data Reg = R0
         | R1
         | R2
         | R3
         | R4
         | R5
         | R6
         | R7
         | R8
         | R9
         | R10
         | R11
         | R12
         | StackPointer
         | LinkRegister
         | PC
         deriving (Eq, Enum)

data Op2 = ShiftReg Reg Shift  -- ^  Shift Register
         | ImmOpInt Int        -- ^  #expression
         | ImmOpCh Char        -- ^  #expression
         deriving (Eq)

data Shift = ASL Int -- ^ Arithmetic Shift Left
           | LSL Int -- ^ Logical Shift Left
           | LSR Int -- ^ Logical Shift Right
           | ASR Int -- ^ Arithmetic Shift Right
           | ROR Int -- ^ Rotate Right
           | RRX     -- ^ Rotate Right Extended
           | NSH     -- ^ No Shift
           deriving (Eq)

data Mem = UB  -- ^ Transfer Byte
         | SB  -- ^ Sign-Extended byte (LDR Only)
         | H   -- ^ Transfer Halfword
         | SH  -- ^ Sign-Extended Halfword (LDR Only)
         | W   -- ^ Word
         deriving (Eq)

data Address = Label String                -- ^ expression
             | Const Int                   -- ^ expression
             | OffReg Reg Offset Bool       -- ^ offset (pre-index)
             deriving (Eq)


data Offset = Int Int             -- ^
            | RegShift Reg Shift  -- ^
            deriving (Eq)

data Condition = Eq   -- ^ Equal
               | Neq  -- ^ Not equal
               | CS   -- ^ Carry set
               | CC   -- ^ Carry clear
               | MI   -- ^ negative
               | PL   -- ^ zero or positive
               | VS   -- ^ overflow
               | VC   -- ^ no overflow
               | HI   -- ^ unsigned higher
               | LS   -- ^ unsigned lower or same
               | GE   -- ^ greater than or equal
               | LTh  -- ^ less than
               | GTh  -- ^ greater than
               | LE   -- ^ less than or equal
               | AL   -- ^ always
               deriving (Eq)

instance PrintARM Instr where
  printARM (Section str)                  = '.' : str  
  printARM (Word i)                       = ".word " ++ show i
  printARM (Ascii str)                    = ".ascii " ++ show str
  printARM Global                         = ".global main"
  printARM (Define str)                   =  str  ++ ":"
  printARM (MOV  cond set reg op2)        = "MOV" ++ printARM(cond) ++ printARM(set)  ++ " "  ++ printARM(reg) ++ ", " ++ printARM(op2)
  printARM (MVN  cond set reg op2)        = "MVN" ++ printARM(cond) ++ printARM(set)  ++ " "  ++ printARM(reg) ++ ", " ++ printARM(op2)
  printARM (B    cond label)              = "B"   ++ printARM(cond) ++ " "  ++ label
  printARM (BL   cond label)              = "BL"  ++ printARM(cond) ++ " "  ++ label
  printARM (CMP  cond reg op2)            = "CMP" ++ printARM(cond) ++ " " ++ printARM (reg) ++ ", " ++ printARM (op2)
  printARM (CMN  cond reg op2)            = "CMN" ++ printARM(cond) ++ " " ++ printARM (reg) ++ ", " ++ printARM (op2)
  printARM (TEQ  cond reg op2)            = "TEQ" ++ printARM(cond) ++ " " ++ printARM (reg) ++ ", " ++ printARM (op2)
  printARM (TST  cond reg op2)            = "TST" ++ printARM(cond) ++ " " ++ printARM (reg) ++ ", " ++ printARM (op2)
  printARM (AND  cond set reg oReg op2)   = "AND" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (EOR  cond set reg oReg op2)   = "EOR" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (BIC  cond set reg oReg op2)   = "BIC" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (ORR  cond set reg oReg op2)   = "ORR" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (SUB  cond set reg oReg op2)   = "SUB" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (RSB  cond set reg oReg op2)   = "RSB" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (ADD  cond set reg oReg op2)   = "ADD" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(op2)
  printARM (MUL  cond set reg oReg oReg1) = "MUL" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(oReg1)
  printARM (MLA  cond set reg oReg oReg1) = "MLA" ++ printARM(cond) ++ printARM (set) ++ " " ++ printARM (reg) ++ ", " ++ printARM(oReg) ++ ", " ++ printARM(oReg1)
  printARM (LDR  cond mem reg address)    = "LDR" ++ printARM(cond) ++ printARM (mem) ++ " " ++ printARM (reg) ++ ", " ++ printARM(address)
  printARM (STR  cond mem reg address)    = "STR" ++ printARM(cond) ++ printARM (mem) ++ " " ++ printARM (reg) ++ ", " ++ printARM(address)
  printARM (PUSH cond regs)               = "PUSH"++ printARM(cond) ++ printARM regs
  printARM (POP  cond regs)               = "POP" ++ printARM(cond) ++ printARM regs
 

instance PrintARM Op2 where
 printARM (ShiftReg reg shift)           = printARM (reg) ++ printARM (shift)
 printARM (ImmOpInt int)                 = "#" ++ show int
 printARM (ImmOpCh char)                 = "#" ++ show char

instance PrintARM Shift where
 printARM (ASL i) = ", ASL #" ++ show i
 printARM (LSL i) = ", LSL #" ++ show i
 printARM (LSR i) = ", LSR #" ++ show i
 printARM (ASR i) = ", ASR #" ++ show i
 printARM (ROR i) = ", ROR #" ++ show i
 printARM (RRX)   = ", RRX"
 printARM (NSH)   = ""

instance PrintARM Mem where
 printARM (UB) = "B"
 printARM (SB) = "SB"
 printARM (H)  = "H"
 printARM (SH) = "SH"
 printARM (W)  = ""

instance PrintARM Address where
 printARM (Label str)              = "=" ++ str
 printARM (Const i)                = "=" ++ show i
 printARM (OffReg reg offset True)  = "[" ++ printARM (reg) ++ printARM (offset) ++ "]" ++ "!"
 printARM (OffReg reg offset False) = "[" ++ printARM (reg) ++ printARM (offset) ++ "]"

instance PrintARM Offset where
 printARM (Int 0)              = ""
 printARM (Int i)              = ", #" ++ show i 
 printARM (RegShift reg shift) = ", " ++ printARM(reg) ++ printARM (shift)

instance PrintARM Condition where
   printARM (Eq) = "EQ"
   printARM (Neq)= "NE"
   printARM (CS) = "CS"
   printARM (CC) = "CC"
   printARM (MI) = "MI"
   printARM (PL) = "PL"
   printARM (VS) = "VS"
   printARM (VC) = "VC"
   printARM (HI) = "HI"
   printARM (LS) = "LS"
   printARM (GE) = "GE"
   printARM (LTh)= "LT"
   printARM (GTh)= "GT"
   printARM (LE) = "LE"
   printARM (AL) = ""

instance PrintARM Reg where
  printARM(R0)  = "r0"
  printARM(R1)  = "r1"
  printARM(R2)  = "r2"
  printARM(R3)  = "r3"
  printARM(R4)  = "r4"
  printARM(R5)  = "r5"
  printARM(R6)  = "r6"
  printARM(R7)  = "r7"
  printARM(R8)  = "r8"
  printARM(R9)  = "r9"
  printARM(R10) = "r10"
  printARM(R11) = "r11"
  printARM(R12) = "r12"
  printARM(StackPointer) = "sp"
  printARM(LinkRegister) = "lr"
  printARM(PC)           = "pc"

instance Bounded Reg where
  minBound = R0
  maxBound = R12

instance PrintARM [Reg] where
 printARM ([])     = []
 printARM (r:rs) = '{' : concat ((printARM r) : [", " ++ printARM reg | reg <- rs]) ++ "}"

instance PrintARM Set where
 printARM(T) = "S"
 printARM(F) = ""
