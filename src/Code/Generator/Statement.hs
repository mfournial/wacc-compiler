module Code.Generator.Statement (
  generate
)
where

import Data.Waskell.ADT
import Code.Instructions()
import Code.Generator.State

generate :: Statement -> ARM Instructions
generate = undefined