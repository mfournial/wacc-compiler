module Code.Generator.Statement (
  generate,
  genScopeBlock
)
where


import Data.Sequence
import Prelude hiding(concat)

import Data.Waskell.ADT
import Code.Instructions()
import Code.Generator.State


generate :: Statement -> ARM Instructions
generate StatSkip = return empty


generate (StatIf (posexp) sb sb') = undefined
generate _ = error "How end up here ???"

genScopeBlock :: ScopeBlock -> ARM Instructions
genScopeBlock (sts, (NewScope scp)) = do
  newEnv
  instructions <- mapM generate (fromList sts)
  closeEnv
  return $ concat instructions
