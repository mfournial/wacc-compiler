module Code.Instructions where

--genUniqueLabel a = unsafePerfomeIO(return $ a ++ getTime)

data Instr = Define String
           | Ret
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
           | PUSH Condition [Regs]
           | POP Condition [Regs]
           | Label String
           deriving (Eq)

data Set = T
         | F
         deriving (Eq)

data Reg = GPRegister Int
         | StackPointer
         | LinkRegister
         | ProgramCounter
         deriving (Eq)

-- ^ (prob not needed for backend but maybe add to data struct {R0 - R4, LR})
data Regs = Registers [Reg]
          | RRange Reg Reg
          deriving (Eq)

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

data Address = StrExp String                -- ^ expression
             | IntExp Int                   -- ^ expression
             | OffReg Reg Offset Bool  -- ^ offset (pre-index)
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


instance Show Instr where
 show (Define str)                   = str   ++ ":"
 show (Ret) = ""
 show (MOV  cond set reg op2)        = "MOV" ++ show(cond) ++ show(set)  ++ " "  ++ show(reg) ++ ", " ++ show(op2)
 show (MVN  cond set reg op2)        = "MVN" ++ show(cond) ++ show(set)  ++ " "  ++ show(reg) ++ ", " ++ show(op2)
 show (B    cond label)              = "B"   ++ show(cond) ++ " "  ++ label
 show (BL   cond label)              = "BL"  ++ show(cond) ++ " "  ++ label
 show (CMP  cond reg op2)            = "CMP" ++ show(cond) ++ " " ++ show (reg) ++ ", " ++ show (op2)
 show (CMN  cond reg op2)            = "CMN" ++ show(cond) ++ " " ++ show (reg) ++ ", " ++ show (op2)
 show (TEQ  cond reg op2)            = "TEQ" ++ show(cond) ++ " " ++ show (reg) ++ ", " ++ show (op2)
 show (TST  cond reg op2)            = "TST" ++ show(cond) ++ " " ++ show (reg) ++ ", " ++ show (op2)
 show (AND  cond set reg oReg op2)   = "AND" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (EOR  cond set reg oReg op2)   = "EOR" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (BIC  cond set reg oReg op2)   = "BIC" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (ORR  cond set reg oReg op2)   = "ORR" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (SUB  cond set reg oReg op2)   = "SUB" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (RSB  cond set reg oReg op2)   = "RSB" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (ADD  cond set reg oReg op2)   = "ADD" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(op2)
 show (MUL  cond set reg oReg oReg1) = "MUL" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(oReg1)
 show (MLA  cond set reg oReg oReg1) = "MLA" ++ show(cond) ++ show (set) ++ " " ++ show (reg) ++ ", " ++ show(oReg) ++ ", " ++ show(oReg1)
 show (LDR  cond mem reg address)    = "LDR" ++ show(cond) ++ show (mem) ++ " " ++ show (reg) ++ ", " ++ show(address)
 show (STR  cond mem reg address)    = "STR" ++ show(cond) ++ show (mem) ++ " " ++ show (reg) ++ ", " ++ show(address)
 show (PUSH cond (r:regs))           = "PUSH"++ show(cond) ++ " {" ++ concat (show (r) : [", " ++ show(reg) | reg <- regs]) ++ "}"
 show (POP  cond (r:regs))           = "POP" ++ show(cond) ++ " {" ++ concat (show (r) : [", " ++ show(reg) | reg <- regs]) ++ "}"


instance Show Op2 where
 show (ShiftReg reg shift)           = show (reg) ++ show (shift)
 show (ImmOpInt int)                 = "#" ++ show (int)
 show (ImmOpCh char)                 = "#" ++ show (char)

instance Show Shift where
 show (ASL int)                       = ", ASL #" ++ show (int)
 show (LSL int)                       = ", LSL #" ++ show (int)
 show (LSR int)                       = ", LSR #" ++ show (int)
 show (ASR int)                       = ", ASR #" ++ show (int)
 show (ROR int)                       = ", ROR #" ++ show (int)
 show (RRX)                           = ", RRX"
 show (NSH)                           = ""

instance Show Mem where
 show (UB)                            = "B"
 show (SB)                            = "SB"
 show (H)                             = "H"
 show (SH)                            = "SH"
 show (W)                             = "W"

instance Show Address where
 show (StrExp str)                    = "=" ++ str
 show (IntExp int)                    = "=" ++ show(int)
 show (OffReg reg offset True)        = "[" ++ show (reg) ++ show (offset) ++ "]" ++ "!"
 show (OffReg reg offset False)        = "[" ++ show (reg) ++ show (offset) ++ "]"

instance Show Offset where
 show (Int int)                       = ", #" ++ show(int)
 show (RegShift reg shift)            = ", " ++ show(reg) ++ show (shift)

instance Show Condition where
 show (Eq)                            = "EQ"
 show (Neq)                           = "NE"
 show (CS)                            = "CS"
 show (CC)                            = "CC"
 show (MI)                            = "MI"
 show (PL)                            = "PL"
 show (VS)                            = "VS"
 show (VC)                            = "VC"
 show (HI)                            = "HI"
 show (LS)                            = "LS"
 show (GE)                            = "GE"
 show (LTh)                            = "LT"
 show (GTh)                            = "GT"
 show (LE)                            = "LE"
 show (AL)                            = ""

instance Show Reg where
 show(GPRegister n)
  | (n <= 10) && (n >= 0)             = "r" ++ show(n)
  | otherwise                         = "error"
 show(StackPointer)                   = "sp"
 show(LinkRegister)                   = "lr"
 show(ProgramCounter)                 = "pc"


instance Show Regs where
 show (Registers (r:rs)) = concat ((show r) : [", " ++ show reg | reg <- rs])
 show (RRange reg oReg)  = show(reg) ++ "-" ++ show(oReg)

instance Show Set where
 show(T) = "S"
 show(F) = ""


--            | LDM Condition AddressMode Reg Regs
--            | STM Condition AddressMode Reg Regs
-- data AddressMode = IA  -- ^ Increment After
--                  | IB  -- ^ Increment Before
--                  | DA  -- ^ Decrement After
--                  | DB  -- ^ Decrement Before
-- 								 deriving (Eq)
