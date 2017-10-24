{-|
This module uses Happy, an automatic parser generator to produce the AST
Module      : Alex.Waskell
Description : Lexical Analyser for WACC
Maintainer  : pvk16@ic.ac.uk
Stability   : experimental
Portability : POSIX

This module should not conflict with libraries and is designed to be imported
qualified. E.g. @import qualified Alex.Waskell as A@
-}
{-

Copyright (C) 2017 Waskell

READ LICENCE FILE

-}

module Alex.Wakell
  (
    getTokens
  ) where

import qualified Data.Waskell as W
import qualified Data.ByteString.Lazy as B

getTokens :: ByteString -> [Token]
getTokens = undefined
