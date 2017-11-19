{-# LANGUAGE TupleSections #-}

module Code.Generator.Expression (expression, expressionReg) where

import Code.Instructions
import Code.Generator.State
import Code.Generator.StateInstructions
import Code.Generator.RetLoc
import Code.Generator.RetLoc.Internal
import Data.Waskell.ADT

import Data.Sequence((><), (<|), (|>), empty, singleton)
import Data.Char(ord)
import Data.Maybe

import Control.Monad

expression :: Expression -> ARM (Instructions, RetLoc)
expression (BracketExp (e, _)) = expression e

expression (IntExp i)         = intToReg i       R0
expression (BoolExp True)     = intToReg 1       R0
expression (BoolExp False)    = intToReg 0       R0
expression (CharExpr c)       = intToReg (ord c) R0
expression PairExpr           = intToReg 0       R0

expression (IdentExpr (s, _)) = fmap (empty,) $ getVar s

expression (UExpr (uexp, _) (e, _)) = do
  (sub, loc)          <- expression e
  strRegIns           <- storeToRegister R0 loc
  return $ (sub >< strRegIns >< evalUExp uexp, (PRL (Register R0)))

expression (BExp (e, _) (bop, _) (e', _)) = do
  (saveReg, _)  <- push [R1]
  (left, lloc)        <- expression e
  strLeft             <- storeToRegister R1 lloc
  (right, rloc)       <- expression e'
  strRight            <- storeToRegister R0 rloc
  resReg              <- pop [R1]
  return $ ((saveReg <| ((left >< strLeft >< right >< strRight >< evalBExp bop) |> resReg)), (PRL (Register R0)))

expression (ArrayExpr (ArrayElem i indexps, _)) = do
  arrLoc <- fmap fromJust $ (getFromHeap . getVal) i
  pushed <- mapM ((pusher =<<) . expression . getVal) indexps 
  ins    <- foldM arrayExp' (storeToRegisterPure R0 arrLoc) $ reverse pushed
  return (ins, (PRL (Register R0)))
  where
    pusher :: (Instructions, RetLoc) -> ARM (Instructions, RetLoc)
    pusher (ins, loc) = do
      str <- storeToRegister R0 loc
      (pushins, pushlocs) <- push [R0]
      return ((ins >< str) |> pushins, head pushlocs)
    arrayExp' :: Instructions -> (Instructions, RetLoc) -> ARM Instructions
    arrayExp' is (is', loc) = do
      str <- storeToRegister R0 loc
      return (is >< is' >< str >< storeToRegisterPure R0 (RegLoc R0)) 

expression (StringExpr str) = fmap ((empty,) . PRL) $ newStringLiteral str

evalUExp :: UnaryOperator -> Instructions
evalUExp UMinus = singleton (RSB AL F R0 R0 (ImmOpInt 0))
evalUExp _ = error "UExp pattern not matched"

evalBExp :: BinaryOperator -> Instructions
evalBExp BTimes = singleton (MUL AL F R0 R0 R1)
evalBExp _ = error "BExp pattern not matched"

intToReg :: Int -> Reg -> ARM (Instructions, RetLoc)
intToReg i r = return (pure (LDR AL W r (Const i)), PRL (Register r))

expressionReg :: Expression -> Reg -> ARM Instructions
expressionReg e r = do
  (is, rl) <- expression e
  iStore <- storeToRegister r rl
  return (is >< iStore)
