module Code.Generator.State (
  Data,
  Junk(..),
  Instructions,
  ARM,
  concat,
  runState,
  execState,
  dataSection,
  newStringLiteral
)
where

import Data.Char(intToDigit)
import Data.Sequence
import Control.Monad.State.Lazy
import Prelude hiding (concat, length, zipWith, null)

import Code.Instructions
-- import Data.Waskell.ADT 

type Instructions = Seq Instr
type ARM = State Junk

newStringLiteral :: String -> ARM ()
newStringLiteral str = state (\junk -> ((), junk{strLits = strLits junk |> str}))

data Junk = Junk {
  strLits :: Data
}

type Data = Seq String

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (size 0 str) |> Ascii str
    labels             = fromList (map (("msg_" ++) . (pure .intToDigit)) [0..])

concat :: Seq (Seq a) -> Seq a
concat = foldl (><) empty

-- | Can't import length from prelude... because of conflicts
size :: String -> Int-> Int
size [] i = return i
size (c : cs) i = size cs (i + 1)

-- labels :: Seq String
-- labels = fromList (map (("msg_" ++) . (pure .intToDigit)) [0..])
-- map (fromList . ("msg_" ++) . (pure intToDigit)) [0..]

-- dataElem :: String -> String -> Instructions
-- dataElem label str = empty |> Define label |> Word (length str) |> Ascii str

