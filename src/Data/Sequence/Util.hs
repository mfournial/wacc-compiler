module Data.Sequence.Util where

import Data.Sequence

concat :: Seq (Seq a) -> Seq a
concat = foldr (><) empty   
{-# INLINE concat #-}
