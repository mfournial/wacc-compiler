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
import qualified Prelude as P

import Code.Instructions
-- import Data.Waskell.ADT 

type Instructions = Seq Instr
type ARM = State Junk

newStringLiteral :: P.String -> ARM ()
newStringLiteral str = state (\junk -> ((), junk{strLits = strLits junk |> str}))

data Junk = Junk {
  strLits:: Data
}

type Data = Seq P.String

dataSection :: Data -> Instructions
dataSection strs
  | null strs = empty
  | P.otherwise = Section "data" <| concat (zipWith dataElem labels strs)
  where
    dataElem label str = empty |> Define label |> Word (P.length str) |> Ascii str
    labels = fromList (P.map (("msg_" P.++) P.. (P.pure P..intToDigit)) [0..])

concat :: Seq (Seq a) -> Seq a
concat = P.foldl (><) empty

-- labels :: Seq P.String
-- labels = fromList (P.map (("msg_" P.++) P.. (P.pure P..intToDigit)) [0..])
-- P.map (fromList . ("msg_" P.++) P.. (pure intToDigit)) [0..]

-- dataElem :: P.String -> P.String -> Instructions
-- dataElem label str = empty |> Define label |> Word (P.length str) |> Ascii str

