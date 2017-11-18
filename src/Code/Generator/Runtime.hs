module Code.Generator.Runtime(RuntimeComponent, generateRuntime) where

import Prelude hiding (concat)

import Data.Sequence
import Data.Sequence.Util

import Code.Instructions(Instructions)

newtype RuntimeComponent = RC Instructions

generateRuntime :: Seq RuntimeComponent -> Instructions
generateRuntime s = concat (fmap (\(RC ins) -> ins) s)
