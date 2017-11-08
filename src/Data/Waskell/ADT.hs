{-|
ADT Structure for the WACC language

Group 26 -- Waskell
Module      : ADT
Maintainer  : mmf115@ic.ac.uk
Portability : POSIX

This module contains the ADT structure of wacc which is used
to check for semantic errors. 

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Waskell.ADT where


import qualified Data.HashMap.Lazy as M


type Position = (Int, Int) -- ^ Position token as of (line, column)
type Pos a = (a, Position) -- ^ Wraps ADT elements with a position token

-- | An ADT element that has a know position in the program file
class Positionable a where
  getPos :: a -> Position -- ^ Extracts the position of an ADT element

instance Positionable (Pos a) where
  getPos = snd

-- | An ADT structure that has can be named in an error
class Referenceable a where
  getName :: a -> String
  getClass :: a -> String

instance Referenceable a => Referenceable (Pos a) where
  getName (a, _) = getName a
  getClass (a, _) = getClass a

-- | List of statements happening in a scope context
type ScopeBlock = ([Statement], NewScope)

-- | A hashmash of element to their types that doesn't treat functions as a 
-- first level element (int x != f(x))
type Scope = M.HashMap String (Either Type Function)

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
    | AssignPairElem (Pos PairElem)
    | AssignCall Identifier [Pos Expression]
  deriving (Eq, Show)

type PairElem = Either (Pos Expression) (Pos Expression)

data Type = Pairable Pairable | PairType Type Type | IOUnit

-- | == Merges Haskell type system with WACC's grammar
instance Eq Type where
  (==) (Pairable (BaseType StringType)) (Pairable (ArrayType (Pairable (BaseType CharType)))) = True
  (==) (Pairable (ArrayType (Pairable (BaseType CharType)))) (Pairable (BaseType StringType)) = True
  (==) (PairType a b) (PairType a' b') = (a == a') && (b == b')
  (==) (PairType _ _) (Pairable PairNull) = True
  (==) (Pairable PairNull) (PairType _ _) = True
  (==) (Pairable a) (Pairable b) = a == b
  (==) IOUnit IOUnit = True
  (==) _ _ = False

instance Show Type where
  show (Pairable (BaseType BoolType)) = "Boolean"
  show (Pairable (BaseType CharType)) = "Char"
  show (Pairable (BaseType StringType)) = "String"
  show (Pairable (BaseType IntType)) = "Int"
  show (Pairable (ArrayType t)) = "[" ++ show t ++ "]"
  show (Pairable (ArrayNull)) = "[]"
  show (Pairable PairNull) = "pair"
  show (PairType a b) = "pair(" ++ show a ++ ", " ++ show b ++ " )"
  show IOUnit = "()"

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Show)

data Pairable = BaseType BaseType | ArrayType Type | ArrayNull | PairNull
  deriving (Show)

instance Eq Pairable where
  (==) ArrayNull (ArrayType _) = True
  (==) (ArrayType _) ArrayNull = True
  (==) (BaseType b) (BaseType b') = b == b'
  (==) (ArrayType a) (ArrayType a') = a == a'
  (==) PairNull PairNull = True
  (==) _ _ = False

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
