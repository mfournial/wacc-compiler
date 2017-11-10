module Code.Instructions where

--genUniqueLabel a = unsafePerfomeIO(return $ a ++ getTime)

data Instr = Define String
		   | Jsr String
		   | Ret
		   | Mov Reg Op
		   | Add Reg Op Op
		   | XOr Op Op Op
		   | Or Reg Reg Op
		   | And Op Op Op
		   | Adds Op Op Op
		   | Sub Op Op Op
		   | Cmp Op Op
		   | Mul Reg Reg Reg Reg
		   | Div Op Op Op
		   | Bra String
		   | LSB Reg Op
		   | Meq Reg Op
		   | Mgt Reg Op
		   | Mlt Reg Op
		   | Mge Reg Op
		   | Mle Reg Op
		   | Mne Reg Op
		   | Neg Reg Reg Op

data Op = RegOp Reg
		| ImmOp Reg Bool
		| OffOp Reg Int
		| IntOp Int

data Condition = Eq   -- ^ Equal
			   | Neq  -- ^ Not equal
			   | CS Bool   -- ^ Carry set
			   | Ov Bool   -- ^ Overflow
			   | GEq -- ^ Greater than or Equal
			   | LEq -- ^ Less than or equal
			   | Lt  -- ^ Less than 
			   | Gt  -- ^ Greater than