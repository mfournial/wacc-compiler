module Code.Generator.Statement (
  generate,
  genScopeBlock
)
where


import Data.Sequence hiding (zip, length)
import Data.Sequence.Util
import Prelude hiding(concat)

import Data.Waskell.ADT
import Code.Instructions
import Code.Generator.Expression
import Code.Generator.State
import Code.Generator.ARM
import Code.Generator.StateInstructions
import Code.Generator.Runtime

import Data.Waskell.Types(unsfType)


generate :: [NewScope] -> Statement -> ARM Instructions
generate _ StatSkip = return empty

generate _ (StatementOperator (StatExit (IntExp i, _), _)) =
  return $ storeToRegisterPure R0 (ImmInt i) |> BL AL "exit"

generate _ (StatementOperator (StatReturn (e, _), _)) =
  (|>) <$> expressionReg e PC <*> pop [PC]

generate ns (StatementOperator (StatPrint (e, _), _)) = do
  (ins, eloc) <- expression e
  strIns      <- storeToRegister R0 eloc
  printrt     <- branchTo $ selectPrint (unsfType e ns)
  return $ (ins >< strIns) |> printrt

generate ns (StatementOperator (StatPrintLn (e, _), _)) = do
  (ins, eloc) <- expression e
  strIns      <- storeToRegister R0 eloc
  printrt     <- branchTo $ selectPrint (unsfType e ns)
  newline     <- newStringLiteral "\\n"
  let strnl   = storeToRegisterPure R0 newline
  printnl     <- branchTo PrintStr
  return $ (((ins >< strIns) |> printrt) >< strnl) |> printnl

generate _ (StatementOperator (StatRead (AssignToIdent _), _)) = do
  return $ empty
  -- TODO check if int or char and call relevant generate functions

--This is slightly inefficient but avoids heavy code duplication TODO: Change implementation from declare then assign to all in one go
generate ns (StatementOperator (StatDecAss t (s, _) ae, p)) = do
  (decin, (loc : [])) <- referencedPush [R0] [s]
  assins              <- assignVar loc ae
  return $ decin <| assins

generate ns (StatementOperator (StatAss (AssignToIdent (i,_)) ae, _)) = do
  loc <- getVar i
  assignVar loc ae

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
  let mallins = storeToRegisterPure R1 (ImmInt bytes) |> BL AL "malloc" 
  let strlent = storeToRegisterPure R0 (ImmInt nwords) >< updateWithRegisterPure R0 (RegLoc R1)
  esinstr <- mapM (\(e,off) -> expression e >>= return . (>< updateWithRegisterPure R0 (RegLocOffset R1 off)) . fst) es
  return $ mallins >< strlent >< mconcat esinstr

assignVar loc _ = error "unimplemented assign"


genScopeBlock :: ScopeBlock -> [NewScope]-> ARM Instructions
genScopeBlock (sts, NewScope scp) ns = do
  newEnv
  instructions <- mapM (generate (NewScope scp : ns)) (fromList sts)
  closeEnv
  return $ concat instructions

selectPrint :: Type -> RCID
selectPrint (PairType a b)                                          = PrintRef
selectPrint (Pairable (BaseType BoolType))                          = PrintBool
selectPrint (Pairable (BaseType StringType))                        = PrintStr
selectPrint (Pairable (BaseType IntType))                           = PrintInt
selectPrint (Pairable (BaseType CharType))                          = PrintChar
selectPrint (Pairable (ArrayType (Pairable (BaseType CharType))))   = PrintStr
selectPrint (Pairable (ArrayType _))                                = PrintRef
selectPrint _                                                       = error "Front end failed to validate types of expressions"
