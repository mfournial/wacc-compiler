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


generate :: Statement -> ARM Instructions
generate StatSkip = return empty

generate (StatementOperator (StatExit (IntExp i, _), _)) = do
  return $ storeToRegisterPure R0 (ImmInt i) |> BL AL "exit"

generate (StatementOperator (StatReturn (e, _), _)) 
  = (|>) <$> expressionReg e PC <*> pop [PC]

generate (StatementOperator ((StatPrint (StringExpr s, _)), _)) = do
  strloc <- newStringLiteral s
  printrt <- branchRuntime generatePrintStrRuntime
  return $ (storeToRegisterPure R0 strloc |> printrt)

generate (StatementOperator ((StatPrint (CharExpr c, _)), _)) = do
  return $ storeToRegisterPure R0 (ImmChar c) |> BL AL "putchar"

generate (StatementOperator ((StatPrint (IntExp i, _)), _)) = do
  printrt <- branchRuntime generatePrintIntRuntime
  return $ storeToRegisterPure R0 (ImmInt i) |> printrt

generate (StatementOperator ((StatPrintLn (StringExpr s, p)), p')) 
  = generate (StatementOperator ((StatPrint (StringExpr (s ++ "\n"), p)), p'))
generate (StatementOperator ((StatPrintLn (IntExp i, _)), _)) = do
  printrt <- branchRuntime generatePrintIntRuntime
  return $ storeToRegisterPure R0 (ImmInt i) |> printrt

generate (StatementOperator ((StatRead (AssignToIdent _)), _)) = do
  return $ empty
  -- TODO check if int or char and call relevant generate functions

generate (StatIf (posexp) sb sb') = do
  (expInstr, loc) <- expression (getVal posexp)
  elseLabel <- nextLabel "else"
  fiLabel <- nextLabel "fi"
  storeIns <- storeToRegister R4 loc
  thenCode <- genScopeBlock sb
  elseCode <- genScopeBlock sb'
  return $ (expInstr
        >< (storeIns
        |> CMP AL R4 (ImmOpInt 1)
        |> B Eq elseLabel)
        >< ((thenCode |> B AL fiLabel)
        >< ((Define elseLabel <| elseCode) |> Define fiLabel)))

generate (StatWhile (posexp) sb) = do
  (expInstr, loc) <- expression (getVal posexp)
  doLabel <- nextLabel "do"
  conditionLabel <- nextLabel "whileCond"
  storeIns <- storeToRegister R4 loc
  bodyCode <- genScopeBlock sb
  return $ (B Eq conditionLabel <| Define doLabel <| bodyCode)
         >< (Define conditionLabel <| expInstr) 
         >< (storeIns
         |> CMP AL R4 (ImmOpInt 1)
         |> B Eq doLabel)

generate _ = error "How end up here ???"

{- Will Jones God code
genScopeBlock' :: ScopeBlock -> ARM Instructions
genScopeBlock'  (sts, NewScope scp)
  = concat <$> withEnv $ \env -> 
      traverse (generate env) (fromList sts)
-}

genScopeBlock :: ScopeBlock -> ARM Instructions
genScopeBlock (sts, (NewScope scp)) = do
  newEnv
  instructions <- mapM generate (fromList sts)
  closeEnv
  return $ concat instructions
