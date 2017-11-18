module Code.Generator.Runtime(RuntimeComponent, generateRuntime) where

import Prelude hiding (concat)

import Data.Sequence
import Data.Sequence.Util

import Code.Instructions(Instructions)
import Code.Generator.State(ARM)

import Code.Generator.Runtime.Internal

generateExitRuntime :: ARM RuntimeComponent
generateExitRuntime = undefined

generateRuntime :: Seq RuntimeComponent -> Instructions
generateRuntime s = concat (fmap (\(RC ins) -> ins) s)
