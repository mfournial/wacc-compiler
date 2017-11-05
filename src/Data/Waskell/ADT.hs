{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Waskell.ADT where

import qualified Data.HashMap.Lazy as M

type Position = (Int, Int)
type Pos a = (a, Position)

class Positionable a where
  getPos :: a -> Position

class Referenceable a where
  getName :: a -> String
  getClass :: a -> String

instance Positionable (Pos a) where
  getPos = snd

instance Referenceable a => Referenceable (Pos a) where
  getName (a, _) = getName a
  getClass (a, _) = getClass a

type ScopeBlock = ([Statement], NewScope)

type Scope = M.HashMap String Type

newtype NewScope = NewScope Scope
  deriving (Eq, Show)

type Identifier = Pos String


newtype WaccTree = WaccTree Program
  deriving (Eq, Show)

data Program = Program [Pos Function] ScopeBlock 
  deriving (Eq, Show)

data Function = Function Type Identifier [Parameter] ScopeBlock
  deriving (Eq, Show)

data Parameter = Param Type Identifier
  deriving (Eq, Show)

data StatementOperator
    = StatDecAss Type Identifier AssignRhs
    | StatAss AssignLhs AssignRhs
    | StatRead AssignLhs
    | StatFree (Pos Expression)
    | StatReturn (Pos Expression)
    | StatExit (Pos Expression)
    | StatPrint (Pos Expression)
    | StatPrintLn (Pos Expression)
  deriving (Eq, Show)

instance Referenceable StatementOperator where
  getClass _                 = "Statement"
  getName (StatDecAss _ _ _) = "="
  getName (StatAss _ _)      = "="
  getName (StatRead _)       = "read"
  getName (StatFree _)       = "free"
  getName (StatReturn _)     = "return"
  getName (StatExit _)       = "exit"
  getName (StatPrint _)      = "print"
  getName (StatPrintLn _)    = "printLn"

data Statement
    = StatSkip
    | StatIf (Pos Expression) ScopeBlock ScopeBlock
    | StatWhile (Pos Expression) ScopeBlock
    | StatScope ScopeBlock
    | StatementOperator (Pos StatementOperator)
  deriving (Eq, Show)

data AssignLhs
    = AssignToIdent Identifier
    | AssignToArrayElem (Pos ArrayElem)
    | AssignToPair (Pos PairElem)
  deriving (Eq, Show)

data AssignRhs
    = AssignExp (Pos Expression)
    | AssignArrayLit ArrayLiteral
    | AssignPair (Pos Expression) (Pos Expression)
    | AssignPairElem PairElem
    | AssignFunctionCall Identifier [Pos Expression]
  deriving (Eq, Show)

type PairElem = Either (Pos Expression) (Pos Expression)

data Type = Pairable Pairable | PairType Type Type | IOUnit
  deriving (Eq, Show)

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Show)

data Pairable = BaseType BaseType | ArrayType Type | PairNull
  deriving (Eq, Show)

-- Note this is a list of expressions, e.g a[1][2][3][4]....
data ArrayElem = ArrayElem Identifier [Pos Expression]
  deriving (Eq, Show)

newtype ArrayLiteral = ArrayLiteral [Pos Expression]
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

data UnaryOperator
    = UBang | UMinus | ULength | UOrd | UChr 
  deriving (Eq, Show)

instance Referenceable UnaryOperator where
  getClass _                 = "Operator"
  getName UBang              = "!"
  getName UMinus             = "-"
  getName ULength            = "len"
  getName UOrd               = "ord"
  getName UChr               = "return"

data BinaryOperator
    = BTimes Position
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

instance Referenceable BinaryOperator where
  getClass _         = "Operator"
  getName BTimes     = "*"
  getName BDivide    = "/"
  getName BModulus   = "%"
  getName BPlus      = "+"
  getName BMinus     = "-"
  getName BMore      = ">"
  getName BLess      = "<"
  getName BMoreEqual = ">="
  getName BLessEqual = "<="
  getName BEqual     = "=="
  getName BNotEqual  = "!="
  getName BAnd       = "&&"
  getName BOr        = "||"
