module Code.Generator.Statement (
  generate,
  genScopeBlock
)
where


import Data.Sequence
import Prelude hiding(concat)

import Data.Waskell.ADT
import Code.Instructions
import Code.Generator.Expression
import Code.Generator.State
import Code.Generator.RetLoc


generate :: Statement -> ARM Instructions
generate StatSkip = return empty

generate (StatIf (posexp) sb sb') = do
  (expInstr, stackOff) <- expression (getVal posexp)
  elseLabel <- nextLabel "else"
  fiLabel <- nextLabel "fi"
  thenCode <- genScopeBlock sb
  elseCode <- genScopeBlock sb'
  return $ (expInstr
        ><( storeToRegister stackOff R1
        |> CMP AL R1 (ImmOpInt 1)
        |> B Eq elseLabel)
        >< ((thenCode |> B AL fiLabel)
        >< ((Define elseLabel <| elseCode) |> Define fiLabel)))
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
