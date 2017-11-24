{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, getArrayEPtr, allocateArray) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.ARM
import Code.Generator.Runtime
import Data.Waskell.ADT

import Data.Sequence ((><), (<|), (|>), empty, singleton)

import Control.Monad

-- | expression will take an expression and produce the ARM instructions required to evaluate it.
-- | POST: Registers R1-R10 will be preserved.
expression :: Expression -> ARM Instructions
expression (BracketExp (e, _)) = expression e

expression (IntExp i)         = intToReg i       R0
expression (BoolExp True)     = intToReg 1       R0
expression (BoolExp False)    = intToReg 0       R0
expression (CharExpr c)       = return $ singleton (MOV AL F R0 (ImmOpCh c))
expression PairExpr           = intToReg 0       R0

expression (IdentExpr (s, _)) = do
  sv <- getStackVar s
  ins <- store R0 sv 
  return ins

expression (UExpr (uexp, _) (e, _)) = (><) <$> expression e <*> evalUExp uexp

expression (BExp (e, _) (bop, _) (e', _)) = do
  (saveReg, _)        <- push [R1]
  left                <- expression e
  (pushleft,_)        <- push [R0]
  right               <- expression e'
  let strRight         = storePure R1 (Register R0) 
  popleft             <- pop [R0]
  bins                <- evalBExp bop
  resReg              <- pop [R1]
  return $ saveReg <| ((left >< (pushleft <| (right >< strRight >< (popleft <| bins)))) |> resReg)

expression (ArrayExpr (ae, _)) = fmap (>< storePure R0 (RegLoc R0)) $ getArrayEPtr ae 
  
expression (StringExpr str) = do
  (savereg, _) <- push [R1, R2]
  let arrayStr = ArrayLiteral $ zip (map CharExpr str) (repeat (0,0))
  instrs <- allocateArray (PRL (Register R2)) arrayStr
  let save = updatePure R2 (Register R0)
  restorereg <- pop [R2, R1]
  return $ savereg <| ((instrs >< save) |> restorereg)

evalUExp :: UnaryOperator -> ARM Instructions
evalUExp UMinus  = branchToIf VS ThrowOverflowErr >>= \e -> return $ singleton (RSB AL T R0 R0 (ImmOpInt 0)) |> e
evalUExp UBang   = return $ singleton (EOR AL F R0 R0 (ImmOpInt 1))
evalUExp ULength = return $ storePure R0 (RegLoc R0)
-- UOrd and UChr actually do nothing! They exist only for type safety.
evalUExp UOrd    = return empty
evalUExp UChr    = return empty

evalBExp :: BinaryOperator -> ARM Instructions

evalBExp BTimes     = do
  let smull = SMULL AL F R0 R1 R0 R1
  let cmp   = CMP AL R1 (ShiftReg R0 (ASR 31))
  checkov  <- branchToIf Neq ThrowOverflowErr
  return $ empty |> smull |> cmp |> checkov 

evalBExp BDivide    = branchTo Checkdbz >>= \b -> return $ singleton b |> BL AL "__aeabi_idiv"
evalBExp BModulus   = branchTo Checkdbz >>= \b -> return $ singleton b |> BL AL "__aeabi_idivmod" |> MOV AL F R0 (ShiftReg R1 NSH) 
evalBExp BPlus      = branchToIf VS ThrowOverflowErr >>= \e -> return $ singleton (ADD AL T R0 R0 (ShiftReg R1 NSH)) |> e
evalBExp BMinus     = branchToIf VS ThrowOverflowErr >>= \e -> return $ singleton (SUB AL T R0 R0 (ShiftReg R1 NSH)) |> e
evalBExp BAnd       = return $ singleton (AND AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BOr        = return $ singleton (ORR AL F R0 R0 (ShiftReg R1 NSH))

evalBExp BMore      = return $ evalBBoolExp GTh LE
evalBExp BLess      = return $ evalBBoolExp LTh GE
evalBExp BMoreEqual = return $ evalBBoolExp GE  LTh
evalBExp BLessEqual = return $ evalBBoolExp LE  GTh
evalBExp BEqual     = return $ evalBBoolExp Eq  Neq
evalBExp BNotEqual  = return $ evalBBoolExp Neq Eq

evalBBoolExp :: Condition -> Condition -> Instructions
evalBBoolExp a b  = singleton (CMP AL R0 (ShiftReg R1 NSH))
                  |> MOV a F R0 (ImmOpInt 1) 
                  |> MOV b F R0 (ImmOpInt 0)


-- | intToReg Places a constant into a register
intToReg :: Int -> Reg -> ARM Instructions
intToReg i r = return $ pure (LDR AL W r (Const i))

-- | getArrayEPtr produces the instructions to store the address of an array element in R0
-- | POST: R1-R10 are preserved
getArrayEPtr :: ArrayElem -> ARM Instructions
getArrayEPtr (ArrayElem (i, _) indexps) = do
  (saveregs, _) <- push [R1, R2]
  pushed <- mapM ((joinedPush =<<) . expression . getVal) indexps 
  let (pushins, pushlocs) = unzip pushed
  ptr  <- getVar i >>= getOffsetFromStackPtr
  let addptr   = ADD AL F R0 StackPointer (ImmOpInt ptr) 
  ins    <- foldM indexIntoArray empty pushlocs
  let restorestack = ADD AL F StackPointer StackPointer (ImmOpInt (4 * Prelude.length pushlocs))
  mapM_ (const decrementStack) pushlocs
  restore <- pop [R2, R1]
  return $ (saveregs <| (mconcat pushins >< singleton addptr >< ins)) |> restorestack |> restore

-- | joinedPush will take a sequence of instructions and return a pair representing 
-- | a list of instructions plus PUSH [R0] along with a location token representing our internal notion
-- | of where the item was pushed.
joinedPush :: Instructions -> ARM (Instructions, Location)
joinedPush ins = do
  (pushins, pushlocs) <- push [R0]
  return (ins |> pushins, head pushlocs)

-- | indexIntoArray takes a reference to a reference to an array
-- | it assumes that the instructions passed to it as a parameter
-- | store the index of an array element in R0
-- | It will then return instructions designed to place a reference to the requried
-- | Array element in R0.
indexIntoArray :: Instructions -> Location -> ARM Instructions
indexIntoArray is loc = do
  checknulls  <- branchTo NullCheck 
  let deref   = storePure R0 (RegLoc R0)
  checknulls' <- branchTo NullCheck
  str <- store R1 loc
  ac  <- branchTo ArrayCheck
  let skiplen = ADD AL F R0 R0 (ImmOpInt 4)
  let strfour = storePure R2 (ImmInt 4)
  let mulins = MUL AL F R1 R1 R2 
  let addins = ADD AL F R0 R0 (ShiftReg R1 NSH)
  return (is >< singleton checknulls >< deref >< singleton checknulls' >< str >< singleton ac >< (skiplen <| (strfour >< (empty |> mulins |> addins)))) 
      
-- | Allocate an array on the heap place the resulting address in loc
allocateArray :: Location
                    -> ArrayLiteral
                    -> ARM Instructions
allocateArray loc (ArrayLiteral pes) = do
  let es      = zip (map getVal pes) (map (4*) [1..length pes])
  let nwords  = length es + 1 -- We need 1 word for the length of the array
  let bytes   = nwords * 4
  let mallins = storePure R0 (ImmInt bytes) |> BL AL "malloc"
  let moveMal = storePure R1 (Register R0)
  assignArr  <- update R1 loc
  let strlent = storePure R0 (ImmInt (length es)) >< updatePure R0 (RegLoc R1)
  esinstr <- mapM (\(e,off) -> expression e >>= return . (>< updatePure R0 (RegLocOffset R1 off))) es
  return $ mallins >< moveMal >< assignArr >< strlent >< mconcat esinstr
