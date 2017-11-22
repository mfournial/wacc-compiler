{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg, getArrayEPtr) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.StateInstructions
import Code.Generator.ARM
import Code.Generator.Runtime
import Data.Waskell.ADT

import Data.Sequence((><), (<|), (|>), empty, singleton)

import Control.Monad


expression :: Expression -> ARM (Instructions, RetLoc)
expression (BracketExp (e, _)) = expression e

expression (IntExp i)         = intToReg i       R0
expression (BoolExp True)     = intToReg 1       R0
expression (BoolExp False)    = intToReg 0       R0
expression (CharExpr c)       = return (singleton (MOV AL F R0 (ImmOpCh c)), PRL (Register R0))
expression PairExpr           = intToReg 0       R0

expression (IdentExpr (s, _)) = do
  sv <- getStackVar s
  ins <- storeToRegister R0 sv 
  return (ins, PRL (Register R0))

expression (UExpr (uexp, _) (e, _)) = do
  (sub, loc)          <- expression e
  strRegIns           <- storeToRegister R0 loc
  return $ (sub >< strRegIns >< evalUExp uexp, (PRL (Register R0)))

expression (BExp (e, _) (bop, _) (e', _)) = do
  (saveReg, _)  <- push [R1]
  (left, lloc)        <- expression e
  strLeft             <- storeToRegister R0 lloc
  (pushleft,_)        <- push [R0]
  (right, rloc)       <- expression e'
  strRight            <- storeToRegister R1 rloc
  popleft             <- pop [R0]
  bins                <- evalBExp bop
  resReg              <- pop [R1]
  return $ ((saveReg <| ((left >< strLeft >< (pushleft <| (right >< strRight >< (popleft <| bins)))) |> resReg)), (PRL (Register R0)))

expression (ArrayExpr (ae, _)) = do
  getptr <- getArrayEPtr ae
  let deref = storeToRegisterPure R0 (RegLoc R0)
  return $ (getptr >< deref, PRL (Register R0))
  
expression (StringExpr str) = fmap ((empty,) . PRL) $ newStringLiteral str

evalUExp :: UnaryOperator -> Instructions
evalUExp UMinus  = singleton (RSB AL F R0 R0 (ImmOpInt 0))
evalUExp UBang   = singleton (EOR AL F R0 R0 (ImmOpInt 1))
evalUExp ULength = storeToRegisterPure R0 (RegLoc R0)
-- UOrd and UChr actually do nothing! They exist only for type safety.
evalUExp UOrd    = empty
evalUExp UChr    = empty

evalBExp :: BinaryOperator -> ARM Instructions
evalBExp BTimes     = return $ singleton (MUL AL F R0 R0 R1)
evalBExp BDivide    = branchTo Checkdbz >>= \b -> return $ singleton b |> BL AL "__aeabi_idiv"
evalBExp BModulus   = branchTo Checkdbz >>= \b -> return $ singleton b |> BL AL "__aeabi_idivmod" |> MOV AL F R0 (ShiftReg R1 NSH) 
evalBExp BPlus      = return $ singleton (ADD AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BMinus     = return $ singleton (SUB AL F R0 R0 (ShiftReg R1 NSH))
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


intToReg :: Int -> Reg -> ARM (Instructions, RetLoc)
intToReg i r = return (pure (MOV AL F r (ImmOpInt i)), PRL (Register r))

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  iStore <- storeToRegister r rl
  return (is >< iStore)

getArrayEPtr :: ArrayElem -> ARM Instructions
getArrayEPtr (ArrayElem (i, _) indexps) = do
  (saveregs, _) <- push [R1, R2]
  pushed <- mapM ((pusher =<<) . expression . getVal) indexps 
  let (pushins, pushlocs) = unzip pushed
  ptr  <- getVar' i id >>= getOffsetFromStackPtr
  let addptr   = ADD AL F R0 StackPointer (ImmOpInt ptr) 
  ins    <- foldM arrayExp' empty $ pushlocs
  let restorestack = ADD AL F StackPointer StackPointer (ImmOpInt (4 * Prelude.length pushlocs))
  mapM_ (\k -> decrementStack) pushlocs
  restore <- pop [R2, R1]
  return $ (saveregs <| (mconcat pushins >< singleton addptr >< ins)) |> restorestack |> restore
  where
    pusher :: (Instructions, RetLoc) -> ARM (Instructions, RetLoc)
    pusher (ins, loc) = do
      str <- storeToRegister R0 loc
      (pushins, pushlocs) <- push [R0]
      return ((ins >< str) |> pushins, head pushlocs)
    arrayExp' :: Instructions -> RetLoc -> ARM Instructions
    arrayExp' is loc = do
      let deref   = storeToRegisterPure R0 (RegLoc R0)
      str <- storeToRegister R1 loc
      ac  <- branchTo ArrayCheck
      let skiplen = ADD AL F R0 R0 (ImmOpInt 4)
      let strfour = storeToRegisterPure R2 (ImmInt 4)
      let mulins = MUL AL F R1 R1 R2 
      let addins = ADD AL F R0 R0 (ShiftReg R1 NSH)
      return (is >< deref >< str >< singleton ac >< (skiplen <| (strfour >< (empty |> mulins |> addins)))) 
