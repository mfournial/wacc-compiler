module Code.Instructions where

--genUniqueLabel a = unsafePerfomeIO(return $ a ++ getTime)

data Instr = Define String
           | Ret
		       | MOV Condition Set Reg Op2
					 | MVN Condition Set Reg Op2
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
					 | LDM Condition AddressMode Reg Regs
					 | STM Condition AddressMode Reg Regs
					 | PUSH Condition Regs
					 | POP Condition Regs
					 | Label String



data Op2 = ShiftReg Reg Shift  -- ^  Shift Reg
         | ImmOp String        -- ^  Expression

data Shift = ASL Int -- ^ Arithmetic Shift Left
           | LSL Int -- ^ Logical Shift Left
					 | LSR Int -- ^ Logical Shift Right
					 | ASR Int -- ^ Arithmetic Shift Right
					 | ROR Int -- ^ Rotate Right
					 | RRX     -- ^ Rotate Right Extended

data Mem = B   -- ^ Transfer Byte
         | SB  -- ^ Sign-Extended byte (LDR Only)
				 | H   -- ^ Transfer Halfword
				 | SH  -- ^ Sign-Extended Halfword (LDR Only)
				 | W   -- ^ Word

data Address = Expression String  -- ^ expression (need to change!!)
	           | OffReg Reg Offset  -- ^ offset (pre-index/ post-index)

data AddressMode = IA  -- ^ Increment After
                 | IB  -- ^ Increment Before
								 | DA  -- ^ Decrement After
								 | DB  -- ^ Decrement Before

data Offset = Int Int            -- ^
            | RegShift Reg Shift -- ^

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
				 | LT   -- ^ less than
				 | GT   -- ^ greater than
				 | LE   -- ^ less than or equal
				 | AL   -- ^ always
