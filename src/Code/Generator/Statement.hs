module Code.Generator.Statement (
  generate,
  genScopeBlock
)
where


import Data.Sequence hiding (zip, length, reverse)
import Data.Sequence.Util
import Prelude hiding (concat)

import Data.Waskell.ADT
import Code.Instructions
import Code.Generator.Expression
import Code.Generator.State
import Code.Generator.ARM
import Code.Generator.StateInstructions
import Code.Generator.Runtime

import Data.Waskell.Types (unsfType)
import qualified Data.HashMap.Strict as M

generate :: [NewScope] -> Statement -> ARM Instructions
generate _ StatSkip = return empty

generate _ (StatementOperator (StatExit (i, _), _)) = do
  (ins, eloc) <- expression i
  strIns      <- storeToRegister R0 eloc
  return $ (ins >< strIns) |> BL AL "exit"

generate _ (StatementOperator (StatReturn (e, _), _)) = do
  ins <- expressionReg e R0
  stck <- fmap (M.size . head . stack) get
  let stackChange = immOpIntCheck (ADD AL F StackPointer StackPointer (ImmOpInt (4 * (stck))))
  return $ ins >< stackChange >< singleton (POP [PC])

generate ns (StatementOperator (StatPrint (e, _), _)) = do
  (ins, eloc) <- expression e
  strIns      <- storeToRegister R0 eloc
  printrt     <- branchTo $ selectPrint (unsfType e ns)
  return $ (ins >< strIns) |> printrt

generate ns (StatementOperator (StatPrintLn (e, _), _)) = do
  (ins, eloc) <- expression e
  strIns      <- storeToRegister R0 eloc
  printrt     <- branchTo $ selectPrint (unsfType e ns)
  newline     <- newStringLiteral "\n"
  let strnl   = storeToRegisterPure R0 newline
  printnl     <- branchTo PrintStr
  return $ (((ins >< strIns) |> printrt) >< strnl) |> printnl

generate ns (StatementOperator ((StatRead (AssignToIdent i@(s,_))), _)) = do
  (StackPtr isp)  <- getStackVar s
  off <- getOffsetFromStackPtr isp
  readchr <- branchTo $ selectReadType(unsfType(IdentExpr i) ns)
  return $ (singleton(ADD AL F  R0 StackPointer (ImmOpInt off))
            |> readchr)

--This is slightly inefficient but avoids heavy code duplication TODO: Change implementation from declare then assign to all in one go
generate ns (StatementOperator (StatDecAss t (s, _) ae, p)) = do
  (decin, (loc : [])) <- referencedPush [R0] [s]
  assins              <- assignVar loc ae
  return $ decin <| assins

generate ns (StatementOperator (StatAss (AssignToIdent (i,_)) ae, _)) = do
  loc <- getStackVar i
  assignVar loc ae

generate ns (StatementOperator (StatAss (AssignToArrayElem (arre, _)) rhs, _)) = do
  getptr <- getArrayEPtr arre
  let movtoten = storeToRegisterPure R10 (Register R0)
  ass <- assignVar (PRL (RegLoc R10)) rhs
  return $ getptr >< movtoten >< ass


generate ns (StatementOperator (StatAss (AssignToPair (Left (e, _), _)) rhs, _)) = do
  (lftins, _) <- expression e
  checkderef  <- branchTo NullCheck
  let movtoten = storeToRegisterPure R10 (Register R0)
  ass <- assignVar (PRL (RegLoc R10)) rhs
  return $ (lftins |> checkderef) >< movtoten >< ass

generate ns (StatementOperator (StatAss (AssignToPair (Right (e, _), _)) rhs, _)) = do
  (rgtins, _) <- expression e
  checkderef  <- branchTo NullCheck
  let movtoten = storeToRegisterPure R10 (Register R0)
  ass <- assignVar (PRL (RegLocOffset R10 4)) rhs
  return $ (rgtins |> checkderef) >< movtoten >< ass

generate ns (StatementOperator (StatFree (IdentExpr (s, _), _), _)) = do
  loc       <- getStackVar s
  ins       <- storeToRegister R0 loc
  checknull <- branchTo NullCheck
  brFree    <- branchTo Free
  let clearReg = MOV AL F R0 (ImmOpInt 0)
  clear  <- updateWithRegister R0 loc
  return $ (ins |> checknull |> brFree |> clearReg) >< clear

generate ns (StatScope sb) = do
  body <- genScopeBlock sb ns
  return $ body

generate ns (StatIf posexp sb sb') = do
  (expInstr, loc) <- expression (getVal posexp)
  elseLabel <- nextLabel "else"
  fiLabel <- nextLabel "fi"
  storeIns <- storeToRegister R4 loc
  thenCode <- genScopeBlock sb ns
  elseCode <- genScopeBlock sb' ns
  return $ expInstr
        >< (storeIns
        |> CMP AL R4 (ImmOpInt 0)
        |> B Eq elseLabel)
        >< ((thenCode |> B AL fiLabel)
        >< ((Define elseLabel <| elseCode) |> Define fiLabel))

generate ns (StatWhile posexp sb) = do
  (expInstr, loc) <- expression (getVal posexp)
  doLabel <- nextLabel "do"
  conditionLabel <- nextLabel "whileCond"
  storeIns <- storeToRegister R4 loc
  bodyCode <- genScopeBlock sb ns
  return $ (B AL conditionLabel <| Define doLabel <| bodyCode)
         >< (Define conditionLabel <| expInstr) 
         >< (storeIns
         |> CMP AL R4 (ImmOpInt 1)
         |> B Eq doLabel)


generate _ _ = error "How end up here ???"

{- Will Jones God code
genScopeBlock' :: ScopeBlock -> ARM Instructions
genScopeBlock'  (sts, NewScope scp)
  = concat <$> withEnv $ \env -> 
      traverse (generate env) (fromList sts)
-}

--PRE: AssignVar does not modify R10, even if it pushes it beforehand
--This is so we may pass it [R10] as a retloc
assignVar :: RetLoc -> AssignRhs -> ARM Instructions
assignVar loc (AssignExp (e, _)) = do
  (ins, eloc)    <- expression e
  strIns         <- storeToRegister    R0 eloc
  strExp         <- updateWithRegister R0 loc
  return $ ins >< strIns >< strExp

assignVar loc (AssignArrayLit (ArrayLiteral pes)) = do
  let es      = zip (map getVal pes) (map (4*) [1..length pes])
  let nwords  = length es + 1 -- We need 1 word for the length of the array
  let bytes   = nwords * 4
  let mallins = storeToRegisterPure R0 (ImmInt bytes) |> BL AL "malloc" 
  let moveMal = storeToRegisterPure R1 (Register R0)
  assignArr  <- updateWithRegister R1 loc
  let strlent = storeToRegisterPure R0 (ImmInt (length es)) >< updateWithRegisterPure R0 (RegLoc R1)
  esinstr <- mapM (\(e,off) -> expression e >>= return . (>< updateWithRegisterPure R0 (RegLocOffset R1 off)) . fst) es
  return $ mallins >< moveMal >< assignArr >< strlent >< mconcat esinstr


assignVar loc (AssignPair (e, _) (e', _)) = do
  let bytes   = 2 * 4  --One word for the value of each expression
  let mallins = storeToRegisterPure R0 (ImmInt bytes) |> BL AL "malloc"
  let moveMal = storeToRegisterPure R1 (Register R0)
  assignPair <- updateWithRegister R1 loc
  assleft    <- expression e >>= return . (>< updateWithRegisterPure R0 (RegLoc R1)) . fst
  assright   <- expression e'>>= return . (>< updateWithRegisterPure R0 (RegLocOffset R1 4)) . fst
  return $ mallins >< moveMal >< assignPair >< assleft >< assright

assignVar loc (AssignPairElem (Left (e, _), _)) = do
  (eins, _) <- expression e
  checkderef  <- branchTo NullCheck
  let getlft = storeToRegisterPure R0 (RegLoc R0)
  assign    <- updateWithRegister R0 loc
  return $ (eins |> checkderef) >< getlft >< assign

assignVar loc (AssignPairElem (Right (e, _), _)) = do
  (eins, _) <- expression e
  checkderef  <- branchTo NullCheck
  let getrgt = storeToRegisterPure R0 (RegLocOffset R0 4)
  assign    <- updateWithRegister R0 loc
  return $ (eins |> checkderef) >< getrgt >< assign

assignVar loc (AssignCall (fname, _) posexprs) = do
  let params = getVal' posexprs
  pushedPars <- mapM evalAndPush params
  result <- updateWithRegister R0 loc
  return $ (mconcat pushedPars 
        |> BL AL ("fun_" ++ fname)
        |> ADD AL F StackPointer StackPointer (ImmOpInt (4 * length params)))
        >< result
  where
    getVal' [] = []
    getVal' (e : es) = getVal' es ++ [getVal e]
    evalAndPush e = expression e >>= \(instr, reg) -> return $ instr |> PUSH [getReg reg]
    getReg (PRL (Register r)) = r
    getReg _ = error "In assignVar (AssignCall): expression didn't return a reg"

genScopeBlock :: ScopeBlock 
              -> [NewScope]
              -> ARM Instructions
genScopeBlock (sts, NewScope scp) ns = do
  newEnv (NewScope scp)
  instructions <- mapM (generate (NewScope scp : ns)) (fromList sts)
  stck <- fmap (M.size . head . stack) get --Exclude number of program args
  mapM_ (\i -> decrementStack) [1..stck]
  closeEnv
  return $ concat instructions >< immOpIntCheck (ADD AL F StackPointer StackPointer (ImmOpInt (4 * stck)))

selectPrint :: Type -> RCID
selectPrint (PairType a b)                                          = PrintRef
selectPrint (Pairable (BaseType BoolType))                          = PrintBool
selectPrint (Pairable (BaseType StringType))                        = PrintStr
selectPrint (Pairable (BaseType IntType))                           = PrintInt
selectPrint (Pairable (BaseType CharType))                          = PrintChar
selectPrint (Pairable (ArrayType (Pairable (BaseType CharType))))   = PrintStr
selectPrint (Pairable (ArrayType _))                                = PrintRef
selectPrint (Pairable (PairNull))                                   = PrintRef
selectPrint _                                                       = error "Front end failed to validate types of expressions"

selectReadType :: Type -> RCID
selectReadType (Pairable(BaseType IntType)) = ReadInt
selectReadType (Pairable(BaseType CharType)) = ReadChar
selectReadType _ = error "front end did not pick this up"

immOpIntCheck :: Instr -> Instructions
immOpIntCheck (ADD cond s reg oReg (ImmOpInt i))
 | i > 1024 = singleton((ADD cond s reg oReg (ImmOpInt (1024)))) >< immOpIntCheck (ADD cond s reg oReg (ImmOpInt (i-1024)))
 | otherwise = singleton(ADD cond s reg oReg (ImmOpInt i))
immOpIntCheck _ = error "should never be here"
