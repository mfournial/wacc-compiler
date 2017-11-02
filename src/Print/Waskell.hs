{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Print.Waskell where

import Bnfc.AbsWacc as AbsWacc
import Data.Char

-- | printTree return a list of strings corresponding to the lines that need to
-- be printed, this allows for adding line numbering later

printTree :: WaccTree 
          -> [String]
printTree = undefined