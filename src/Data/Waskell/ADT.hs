{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Waskell.ADT where

--import qualified Data.Waskell.ADTHappy as H
import qualified Data.HashMap.Lazy as M

type Position = (Int, Int)
type Pos a = (a, Position)

class Positionable a where
  getPos :: a -> Position

instance Positionable (Pos a) where
  getPos = snd

type ScopeBlock = ([Statement], NewScope)

type Scope = M.HashMap String Type

newtype NewScope = NewScope Scope
  deriving (Eq, Show)

type Identifier = Pos String


data WaccTree = WaccTree Program
  deriving (Eq, Show)

data Program = Program [Pos Function] ScopeBlock 
  deriving (Eq, Show)

data Function = Function Type Identifier [Parameter] ScopeBlock
  deriving (Eq, Show)

data Parameter = Param Type Identifier
  deriving (Eq, Show)

data StatementOperator
    = StatDecAss Type Identifier (Pos AssignRhs)
    | StatAss (Pos AssignLhs) (Pos AssignRhs)
    | StatRead (Pos AssignLhs)
    | StatFree (Pos Expression)
    | StatReturn (Pos Expression)
    | StatExit (Pos Expression)
    | StatPrint (Pos Expression)
    | StatPrintLn (Pos Expression)
  deriving (Eq, Show)

data Statement
    = StatSkip
    | StatIf (Pos Expression) ScopeBlock ScopeBlock
    | StatWhile (Pos Expression) ScopeBlock
    | StatScope ScopeBlock
    | StatementOperator (Pos StatementOperator)
  deriving (Eq, Show)


data AssignLhs
    = AssignToIdent (Pos String)
    | AssignToArrayElem (Pos ArrayElem)
    | AssignToPair PairElem
  deriving (Eq, Show)


data AssignRhs
    = AssignExp (Pos Expression)
    | AssignArrayLit (Pos ArrayLiteral)
    | AssignPair (Pos Expression) (Pos Expression)
    | AssignPairElem PairElem
    | AssignFunctionCall Identifier [Expression]
  deriving (Eq, Show)

type PairElem = Either (Pos Expression) (Pos Expression)

data Type = Pairable Pairable | PairType Type Type
  deriving (Eq, Show)

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Show)

data Pairable = BaseType BaseType | ArrayType Type | PairNull
  deriving (Eq, Show)

-- Note this is a list of expressions, e.g a[1][2][3][4]....
data ArrayElem = ArrayElem Identifier [Pos Expression]
  deriving (Eq, Show)

data ArrayLiteral = ArrayLiteral [Pos Expression]
  deriving (Eq, Show)


data Expression
    = IntExp Int
    | BoolExp Bool
    | CharExpr Char
    | StringExpr String
    | PairExpr
    | IdentExpr Identifier
    | ArrayExpr (Pos ArrayElem)
    | UExpr (Pos UnaryOperator) (Pos Expression)
    | BExp (Pos Expression) (Pos BinaryOperator) (Pos Expression)
    | BracketExp (Pos Expression)
  deriving (Eq, Show)

type UnaryOperatorP = (UnaryOperator, Position)

data UnaryOperator
    = UBang | UMinus | ULenght | UOrd | UChr 
  deriving (Eq, Show)

data BinaryOperator
    = BTimes
    | BDivide
    | BModulus
    | BPlus
    | BMinus
    | BMore
    | BLess
    | BMoreEqual
    | BLessEqual
    | BEqual
    | BNotEqual
    | BAnd
    | BOr
  deriving (Eq, Show)
