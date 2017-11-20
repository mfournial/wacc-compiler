module Code.Generator.Statement (
  generate,
  genScopeBlock
)
where


import Data.Sequence
import Data.Sequence.Util
import Prelude hiding(concat)

import Data.Waskell.ADT
import Code.Instructions
import Code.Generator.Expression
import Code.Generator.State
import Code.Generator.RetLoc
import Code.Generator.StateInstructions
import Code.Generator.Runtime


generate :: [NewScope] -> Statement -> ARM Instructions
generate _ StatSkip = return empty

generate _ (StatementOperator (StatExit (IntExp i, _), _)) = do
  return $ storeToRegisterPure R0 (ImmInt i) |> BL AL "exit"

generate _ (StatementOperator (StatReturn (e, _), _)) 
  = (|>) <$> expressionReg e PC <*> pop [PC]

generate _ (StatementOperator ((StatPrint (StringExpr s, _)), _)) = do
  strloc <- newStringLiteral s
  printrt <- branchRuntime generatePrintStrRuntime
  return $ (storeToRegisterPure R0 strloc |> printrt)

generate _ (StatementOperator ((StatPrint (CharExpr c, _)), _)) = do
  return $ storeToRegisterPure R0 (ImmChar c) |> BL AL "putchar"

generate _ (StatementOperator ((StatPrint (IntExp i, _)), _)) = do
  printrt <- branchRuntime generatePrintIntRuntime
  return $ storeToRegisterPure R0 (ImmInt i) |> printrt

--Incorrect behaviour whoops! We threw type information away.
generate _ (StatementOperator ((StatPrint (e, _)), _)) = do
  (ins, eloc) <- expression e
  strIns      <- storeToRegister R0 eloc
  printrt     <- branchRuntime generatePrintIntRuntime
  return $ (ins >< strIns) |> printrt  

generate ns (StatementOperator ((StatPrintLn (StringExpr s, p)), p')) 
  = generate ns (StatementOperator ((StatPrint (StringExpr (s ++ "'\\n"), p)), p'))

  -- TODO REMOVE IRRELEVANT calls to print
generate _ (StatementOperator ((StatPrintLn (IntExp i, _)), _)) = do
  printrt <- branchRuntime generatePrintIntRuntime
  return $ storeToRegisterPure R0 (ImmInt i) |> printrt

generate _ (StatementOperator ((StatRead (AssignToIdent _)), _)) = do
  return $ empty
  -- TODO check if int or char and call relevant generate functions

generate _ (StatementOperator ((StatDecAss (Pairable (BaseType StringType)) _ _), _)) = undefined
generate _ (StatementOperator ((StatDecAss (Pairable (BaseType b)) (iid, _) (AssignExp (e, _))), _)) = do
  (ins, eloc)    <- expression e
  strIns         <- storeToRegister R0 eloc
  strExp         <- referencedPush [R0] [iid]
  return $ (ins >< strIns) |> strExp

generate _ (StatementOperator ((StatDecAss t (iid, _) arhs), _)) = undefined
  

generate ns (StatIf (posexp) sb sb') = do
  (expInstr, loc) <- expression (getVal posexp)
  elseLabel <- nextLabel "else"
  fiLabel <- nextLabel "fi"
  storeIns <- storeToRegister R4 loc
  thenCode <- genScopeBlock sb ns
  elseCode <- genScopeBlock sb' ns
  return $ (expInstr
        >< (storeIns
        |> CMP AL R4 (ImmOpInt 0)
        |> B Eq elseLabel)
        >< ((thenCode |> B AL fiLabel)
        >< ((Define elseLabel <| elseCode) |> Define fiLabel)))

generate ns (StatWhile (posexp) sb) = do
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

genScopeBlock :: ScopeBlock -> [NewScope]-> ARM Instructions
genScopeBlock (sts, (NewScope scp)) ns = do
  newEnv
  instructions <- mapM (generate ((NewScope scp):ns)) (fromList sts)
  closeEnv
  return $ concat instructions
