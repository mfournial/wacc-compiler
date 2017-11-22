{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.StateInstructions
import Code.Generator.ARM
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
  resReg              <- pop [R1]
  return $ ((saveReg <| ((left >< strLeft >< (pushleft <| (right >< strRight >< (popleft <| evalBExp bop)))) |> resReg)), (PRL (Register R0)))

expression (ArrayExpr (ArrayElem (i, _) indexps, _)) = do
  (saveregs, _) <- push [R1, R2]
  sv <- getStackVar i
  pushed <- mapM ((pusher =<<) . expression . getVal) indexps 
  let (pushins, pushlocs) = unzip pushed
  strPtr <- storeToRegister R0 sv
  ins    <- foldM arrayExp' empty $ pushlocs
  let restorestack = ADD AL F StackPointer StackPointer (ImmOpInt (4 * length pushlocs))
  restore <- pop [R1, R2]
  return ((saveregs <| (mconcat pushins >< strPtr >< ins)) |> restorestack |> restore, (PRL (Register R0)))
  where
    pusher :: (Instructions, RetLoc) -> ARM (Instructions, RetLoc)
    pusher (ins, loc) = do
      str <- storeToRegister R0 loc
      (pushins, pushlocs) <- push [R0]
      return ((ins >< str) |> pushins, head pushlocs)
    arrayExp' :: Instructions -> RetLoc -> ARM Instructions
    arrayExp' is loc = do
      let skiplen = ADD AL F R0 R0 (ImmOpInt 4)
      let strfour = storeToRegisterPure R2 (ImmInt 4)
      str <- storeToRegister R1 loc
      let mulins = MUL AL F R1 R1 R2 
      let addins = ADD AL F R0 R0 (ShiftReg R1 NSH)
      return (is >< (skiplen <| (strfour >< str >< (empty |> mulins |> addins) >< storeToRegisterPure R0 (RegLoc R0)))) 

expression (StringExpr str) = fmap ((empty,) . PRL) $ newStringLiteral str

evalUExp :: UnaryOperator -> Instructions
evalUExp UMinus  = singleton (RSB AL F R0 R0 (ImmOpInt 0))
evalUExp UBang   = singleton (EOR AL F R0 R0 (ImmOpInt 1))
evalUExp ULength = storeToRegisterPure R0 (RegLoc R0)
-- UOrd and UChr actually do nothing! They exist only for type safety.
evalUExp UOrd    = empty
evalUExp UChr    = empty

evalBExp :: BinaryOperator -> Instructions
evalBExp BTimes   = singleton (MUL AL F R0 R0 R1)
--Todo check div by zero
evalBExp BDivide    = singleton (BL AL "__aeabi_idiv") 
evalBExp BModulus   = singleton (BL AL "__aeabi_idivmod") |> MOV AL F R0 (ShiftReg R1 NSH) 
evalBExp BPlus      = singleton (ADD AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BMinus     = singleton (SUB AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BAnd       = singleton (AND AL F R0 R0 (ShiftReg R1 NSH))
evalBExp BOr        = singleton (ORR AL F R0 R0 (ShiftReg R1 NSH))

evalBExp BMore      = evalBBoolExp GTh LE
evalBExp BLess      = evalBBoolExp LTh GE
evalBExp BMoreEqual = evalBBoolExp GE  LTh
evalBExp BLessEqual = evalBBoolExp LE  GTh
evalBExp BEqual     = evalBBoolExp Eq  Neq
evalBExp BNotEqual  = evalBBoolExp Neq Eq

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
