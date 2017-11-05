{-# LANGUAGE TypeFamilies #-}

module Data.Waskell.ADT where

--import qualified Data.Waskell.ADTHappy as H
import qualified Data.HashMap.Lazy as M

type family Positionable a where
  Positionable a = (a, Position)

getPos :: Positionable a -> Position
getPos = snd

type ScopeBlock = ([Statement], NewScope)
type Position = (Int, Int)

type Scope = M.HashMap String Type

newtype NewScope = NewScope Scope
  deriving (Eq, Show)

newtype Identifier = Identifier (Position,String)
  deriving (Eq, Show)

newtype WaccTree = WaccTree Program
  deriving (Eq, Show)

data Program = Program [Function] ScopeBlock 
  deriving (Eq, Show)

data Function = Function Type Identifier [Parameter] ScopeBlock
  deriving (Eq, Show)

data Parameter = Param Type Identifier Position
  deriving (Eq, Show)

data Statement
    = StatSkip
    | StatDecAss Type Identifier AssignRhs
    | StatAss AssignLhs AssignRhs 
    | StatRead AssignLhs
    | StatFree Expression
    | StatReturn Expression
    | StatExit Expression
    | StatPrint Expression
    | StatPrintLn Expression
    | StatIf Expression ScopeBlock ScopeBlock
    | StatWhile Expression ScopeBlock
    | StatScope ScopeBlock
  deriving (Eq, Show)

type StatementP = (Statement, Position)

type AssignLhsP = (AssignLhs, Position)

data AssignLhs
    = AssignToIdent Identifier
    | AssignToArrayElem ArrayElem
    | AssignToPair PairElem
  deriving (Eq, Show)

type AssignRhsP = (AssignRhs, Position)

data AssignRhs
    = AssignExp Expression 
    | AssignArrayLit ArrayLiteral
    | AssignPair Expression Expression
    | AssignPairElem PairElem
    | AssignFunctionCall Identifier [Expression]
  deriving (Eq, Show)

type PairElem = (Either Expression Expression, Position)

data Type = Pairable Pairable | PairType Type Type
  deriving (Eq, Show)

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Show)

data Pairable = BaseType BaseType | ArrayType Type | PairNull
  deriving (Eq, Show)

-- Note this is a list of expressions, e.g a[1][2][3][4]....
data ArrayElem = ArrayElem Identifier [Expression]
  deriving (Eq, Show)

newtype ArrayLiteral = ArrayLiteral [Expression]
  deriving (Eq, Show)


type ExpressionP = (Expression, Position)

data Expression
    = IntExp Int
    | BoolExp Bool
    | CharExpr Char
    | StringExpr String
    | PairExpr
    | IdentExpr Identifier
    | ArrayExpr ArrayElem
    | UExpr UnaryOperator Expression
    | BExp Expression BinaryOperator Expression
    | BracketExp Expression
  deriving (Eq, Show)

type UnaryOperatorP = (UnaryOperator, Position)

data UnaryOperator
    = UBang | UMinus | ULenght | UOrd | UChr 
  deriving (Eq, Show)

type BinaryOperatorP = (BinaryOperator, Position)

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
