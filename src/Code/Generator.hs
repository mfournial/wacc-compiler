module Code.Generator (produceASM) where

import Data.Foldable (toList)
import Data.Sequence
import Prelude hiding (concat)

import Code.Generator.State
import qualified Code.Generator.Statement as StatementARM
import Code.Instructions
import Data.Waskell.ADT 

produceASM :: FilePath -> WaccTree -> IO ()
-- produceASM f = writeCode f . genCode 
produceASM = (.genCode) . writeCode

genCode :: WaccTree -> Seq Instr
genCode t = (dataSection (strLits st) |> DIVIDER |> DIVIDER) >< instr
  where
    (instr, st) = runState (genCode' t) newState 

genCode' :: WaccTree -> ARM Instructions
genCode' (WaccTree (Program fps sb)) = do
  finstr <- genFuncsCode (fromList $ map getVal fps)
  minstr <- genScopeBlock sb
  return $ ((empty
           |> Section "text")
           >< (finstr
           |> Global |> Define "main" |> PUSH [LinkRegister]))
           >< (minstr
           |> LDR AL W R0 (Const 0) |> POP [PC] |> FunSection "ltorg")
  where
    genFuncsCode :: Seq Function -> ARM Instructions
    genFuncsCode fs = fmap concat $ mapM genFuncCode fs

genFuncCode :: Function -> ARM Instructions
genFuncCode (Function _ iden params sb) = do
  mapM_ push $ map (\(Param _ identifier) -> getVal identifier) params
  body <- genScopeBlock sb
  return $ (Define ("fun_" ++ (getVal iden)) <|) body |> DIVIDER

  --   (instr, state) = runState genInstrs []
  --   genFuncsCode :: ARM ([Function] -> Seq Instr)
  --   genFuncsCode = undefined
  --   concatMap ((genFuncCode state) . getVal) fs

genScopeBlock :: ScopeBlock -> ARM Instructions
genScopeBlock (sts, (NewScope scp)) = do
  newEnv
  instructions <- mapM (StatementARM.generate) (fromList sts)
  closeEnv
  return $ concat instructions

writeCode :: FilePath -> Instructions -> IO ()
writeCode = (. (unlines . toList . fmap printARM)) . writeFile
