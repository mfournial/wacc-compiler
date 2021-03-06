module Code.Generator.Statement (
  genScopeBlock
)
where


import qualified Data.HashMap.Strict as M
import Data.Sequence hiding (zip, length, reverse, update)
import Data.Sequence.Util
import Prelude hiding (concat)

import Data.Waskell.ADT
import Code.Generator.ARM
import Code.Generator.Expression
import Code.Generator.State
import Code.Generator.Runtime
import Code.Instructions


-- | Generate takes a statement and then produces the arm instructions required to execute the
-- | statement. Note generate makes no guarentees about the registers that it uses and in
-- | particular will make use of R0 and R1 liberally.
-- |
-- | Sequences are used for efficiency reasons, in order to avoid O(n) insertion on the production
-- | of each instruction.
generate :: Statement -> ARM Instructions
generate StatSkip = return empty

generate (StatementOperator (StatExit (i, _), _)) = fmap (|> BL AL "exit") $ expression i

generate (StatementOperator (StatReturn (e, _), _)) = do
  ins <- expression e
  stck <- fmap (M.size . head . stack) get
  let stackChange = immOpIntCheck (ADD AL F StackPointer StackPointer (ImmOpInt (4 * (stck))))
  return $ ins >< stackChange >< singleton (POP [PC])

generate (StatementOperator (StatPrint (e, _), _)) = do
  ins         <- expression e
  t <- lookupType e
  printrt     <- branchTo $ selectPrint t
  return $ ins |> printrt

generate (StatementOperator (StatPrintLn (e, _), _)) = do
  ins         <- expression e
  t           <- lookupType e
  printrt     <- branchTo $ selectPrint t
  newline     <- newStringLiteral "\n"
  let strnl   = storePure R0 newline
  printnl     <- branchTo PrintStr
  return $ ((ins |> printrt) >< strnl) |> printnl

generate (StatementOperator ((StatRead (AssignToIdent i@(s,_))), _)) = do
  (StackPtr isp)  <- getStackVar s
  off <- getOffsetFromStackPtr isp
  t   <- lookupType (IdentExpr i)
  readchr <- branchTo $ selectReadType t
  return $ (singleton(ADD AL F  R0 StackPointer (ImmOpInt off))
            |> readchr)

generate (StatementOperator ((StatRead (AssignToArrayElem (arre, p)), _))) = do
  getptr <- getArrayEPtr arre
  t      <- lookupType (ArrayExpr (arre,p))
  readci <- branchTo $ selectReadType(t)
  return $ getptr |> readci 


generate (StatementOperator (StatRead (AssignToPair(Left (e@(IdentExpr (s,_)),_), _)), _)) = do
   regs        <- getStackVar s
   strregs     <- store R0 regs 
   checkderef  <- branchTo NullCheck
   readchr     <- (getPairTypeL e)
   return $ (strregs |> checkderef) |> readchr

generate (StatementOperator (StatRead (AssignToPair(Right (e@(IdentExpr (s,_)),_), _)), _)) = do
   regs        <- getStackVar s
   strregs     <- store R0 regs 
   checkderef  <- branchTo NullCheck
   readchr     <- (getPairTypeR e)
   return $ (strregs |> checkderef |> (ADD AL F R0 R0 (ImmOpInt 4))) |> readchr

--This is slightly inefficient but avoids heavy code duplication TODO: Change implementation from declare then assign to all in one go
generate (StatementOperator (StatDecAss t (s, _) ae, p)) = do
  (decin, (loc : [])) <- referencedPush [R0] [s]
  assins              <- assignVar loc ae
  return $ decin <| assins

generate (StatementOperator (StatAss (AssignToIdent (i,_)) ae, _)) = getStackVar i >>= \l -> assignVar l ae

generate (StatementOperator (StatAss (AssignToArrayElem (arre, _)) rhs, _)) = do
  getptr <- getArrayEPtr arre
  let movtoten = storePure R10 (Register R0)
  ass <- assignVar (PRL (RegLoc R10)) rhs
  return $ getptr >< movtoten >< ass


generate (StatementOperator (StatAss (AssignToPair (Left (e, _), _)) rhs, _)) = do
  lftins      <- expression e
  checkderef  <- branchTo NullCheck
  let movtoten = storePure R10 (Register R0)
  ass <- assignVar (PRL (RegLoc R10)) rhs
  return $ (lftins |> checkderef) >< movtoten >< ass

generate (StatementOperator (StatAss (AssignToPair (Right (e, _), _)) rhs, _)) = do
  rgtins      <- expression e
  checkderef  <- branchTo NullCheck
  let movtoten = storePure R10 (Register R0)
  ass <- assignVar (PRL (RegLocOffset R10 4)) rhs
  return $ (rgtins |> checkderef) >< movtoten >< ass

generate (StatementOperator (StatFree (IdentExpr (s, _), _), _)) = do
  loc       <- getStackVar s
  ins       <- store R0 loc
  brFree    <- branchTo Free
  let clearReg = MOV AL F R0 (ImmOpInt 0)
  clear  <- update R0 loc
  return $ (ins |> brFree |> clearReg) >< clear

generate (StatScope sb) = genScopeBlock sb

generate (StatIf posexp sb sb') = do
  expInstr  <- expression (getVal posexp)
  elseLabel <- nextLabel "else"
  fiLabel <- nextLabel "fi"
  let storeIns = storePure R4 (Register R0)
  thenCode <- genScopeBlock sb
  elseCode <- genScopeBlock sb'
  return $ expInstr
        >< (storeIns
        |> CMP AL R4 (ImmOpInt 0)
        |> B Eq elseLabel)
        >< ((thenCode |> B AL fiLabel)
        >< ((Define elseLabel <| elseCode) |> Define fiLabel))

generate (StatWhile posexp sb) = do
  expInstr  <- expression (getVal posexp)
  doLabel <- nextLabel "do"
  conditionLabel <- nextLabel "whileCond"
  let storeIns = storePure R4 (Register R0)
  bodyCode <- genScopeBlock sb
  return $ (B AL conditionLabel <| Define doLabel <| bodyCode)
         >< (Define conditionLabel <| expInstr) 
         >< (storeIns
         |> CMP AL R4 (ImmOpInt 1)
         |> B Eq doLabel)


generate _ = error "How end up here ???"

-- | AssignVar takes a location/address and an assignrhs and will produce instructions
-- | to evaluate the rhs and then put the result in the location/address passed.
-- | assignVar again makes use of registers R0 and R1 liberally.
-- | Note though it guarentees that R10 is perserved, in case the user wishes to store
-- | The result in a register.
--PRE: AssignVar does not modify R10, even if it pushes it beforehand
--This is so we may pass it [R10] as a retloc
assignVar :: Location -> AssignRhs -> ARM Instructions
assignVar loc (AssignExp (e, _)) = (><) <$> expression e <*> update R0 loc

assignVar loc (AssignArrayLit al) = allocateArray loc al

assignVar loc (AssignPair (e, _) (e', _)) = do
  let bytes   = 2 * 4  --One word for the value of each expression
  let mallins = storePure R0 (ImmInt bytes) |> BL AL "malloc"
  let moveMal = storePure R1 (Register R0)
  assignPair <- update R1 loc
  assleft    <- expression e >>= return . (>< updatePure R0 (RegLoc R1))
  assright   <- expression e'>>= return . (>< updatePure R0 (RegLocOffset R1 4))
  return $ mallins >< moveMal >< assignPair >< assleft >< assright

assignVar loc (AssignPairElem (Left (e, _), _)) = do
  eins  <- expression e
  checkderef  <- branchTo NullCheck
  let getlft = storePure R0 (RegLoc R0)
  assign    <- update R0 loc
  return $ (eins |> checkderef) >< getlft >< assign

assignVar loc (AssignPairElem (Right (e, _), _)) = do
  eins <- expression e
  checkderef  <- branchTo NullCheck
  let getrgt = storePure R0 (RegLocOffset R0 4)
  assign    <- update R0 loc
  return $ (eins |> checkderef) >< getrgt >< assign

assignVar loc (AssignCall (fname, _) posexprs) = do
  let params = getVal' posexprs
  pushedPars <- mapM evalAndPush params
  result <- update R0 loc
  return $ (mconcat pushedPars 
        |> BL AL ("fun_" ++ fname)
        |> ADD AL F StackPointer StackPointer (ImmOpInt (4 * length params)))
        >< result
  where
    getVal' [] = []
    getVal' (e : es) = getVal' es ++ [getVal e]
    evalAndPush e = expression e >>= \instr -> return $ instr |> PUSH [R0]

-- | genScopeBlock takes a scopeblock and produces the instructions required to execute 
-- | all of the wacc statements in the scopeblock.
genScopeBlock :: ScopeBlock 
              -> ARM Instructions
genScopeBlock (sts, NewScope scp) = do
  newEnv (NewScope scp)
  instructions <- mapM generate (fromList sts)
  stck <- fmap (M.size . head . stack) get --Exclude number of program args
  mapM_ (\i -> decrementStack) [1..stck]
  closeEnv
  return $ concat instructions >< immOpIntCheck (ADD AL F StackPointer StackPointer (ImmOpInt (4 * stck)))


getPairTypeL :: Expression -> ARM Instr
getPairTypeL e = do
 t <- lookupType e
 case t of
  (PairType l _) -> branchTo $ (selectReadType l)
  _   -> error "front end failed"


getPairTypeR :: Expression -> ARM Instr
getPairTypeR e = do
 t <- lookupType e
 case t of
  (PairType _ r) -> branchTo $ (selectReadType r)
  _   -> error "front end failed"


-- | SelectPrint takes a type and then returns the id of the runtime element used to print
-- | that types string representation.
selectPrint :: Type -> RCID
selectPrint (PairType a b)                                          = PrintRef
selectPrint (Pairable (BaseType BoolType))                          = PrintBool
selectPrint (Pairable (BaseType StringType))                        = PrintCharArray
selectPrint (Pairable (BaseType IntType))                           = PrintInt
selectPrint (Pairable (BaseType CharType))                          = PrintChar
selectPrint (Pairable (ArrayType (Pairable (BaseType CharType))))   = PrintCharArray
selectPrint (Pairable (ArrayType _))                                = PrintRef
selectPrint (Pairable (PairNull))                                   = PrintRef
selectPrint _                                                       = error "Front end failed to validate types of expressions"

-- | SelectReadType takes a type and then returns the id of the runtime element used to
-- | read that types string representation.
selectReadType :: Type -> RCID
selectReadType (Pairable(BaseType IntType)) = ReadInt
selectReadType (Pairable(BaseType CharType)) = ReadChar
selectReadType _ = error "front end did not pick this up"

immOpIntCheck :: Instr -> Instructions
immOpIntCheck (ADD cond s reg oReg (ImmOpInt i)) = immOpIntCheck' ADD cond s reg oReg i
immOpIntCheck (SUB cond s reg oReg (ImmOpInt i)) = immOpIntCheck' SUB cond s reg oReg i
immOpIntCheck (RSB cond s reg oReg (ImmOpInt i)) = immOpIntCheck' RSB cond s reg oReg i
immOpIntCheck (AND cond s reg oReg (ImmOpInt i)) = immOpIntCheck' AND cond s reg oReg i
immOpIntCheck (EOR cond s reg oReg (ImmOpInt i)) = immOpIntCheck' EOR cond s reg oReg i
immOpIntCheck (BIC cond s reg oReg (ImmOpInt i)) = immOpIntCheck' BIC cond s reg oReg i
immOpIntCheck (ORR cond s reg oReg (ImmOpInt i)) = immOpIntCheck' ORR cond s reg oReg i
immOpIntCheck _ = error "should never be here"

immOpIntCheck' :: (Condition -> Set -> Reg -> Reg -> Op2 -> Instr) -> Condition -> Set -> Reg -> Reg -> Int -> Instructions
immOpIntCheck' constr cond s reg oReg i
 | i > 1024 = singleton((constr cond s reg oReg (ImmOpInt (1024)))) >< immOpIntCheck' constr cond s reg reg (i-1024)
 | otherwise = singleton(constr cond s reg oReg (ImmOpInt i))
