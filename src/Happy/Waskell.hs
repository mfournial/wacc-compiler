{-|
This module uses Happy, an automatic parser generator to produce the AST
Module      : Happy.Waskell
Description : Parser generator for WACC
Maintainer  : mmf115@ic.ac.uk
Stability   : experimental
Portability : POSIX

This module should not conflict with libraries and is designed to be imported
qualified. E.g. @import qualified Happy.Waskell as H@
-}
{-

Copyright (C) 2017 Waskell

READ LICENCE FILE

-}

module Happy.Wakell
  (
    generateAST
  ) where

import qualified Data.Waskell as W
import qualified Data.ByteString.Lazy as B

generateAST :: ByteString -> WaccTree
generateAST = undefined
