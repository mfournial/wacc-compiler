module Data.Waskell.ADT where

import Data.Waskell.Scope

type ScopeBlock = ([Statement], NewScope)

newtype IntDigit = IntDigit ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PlusLiteral = PlusLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype MinusLiteral = MinusLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BoolLiteral = BoolLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype CharLiteral = CharLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PairLiteral = PairLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Identifier = Identifier ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype StringLiteral = StringLiteral ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
data WaccTree = WaccTree Program
  deriving (Eq, Ord, Show, Read)

data Program = Program [Function] [Statement] NewScope
  deriving (Eq, Ord, Show, Read)

data Function = Function Type Identifier [Parameter] ScopeBlock
  deriving (Eq, Ord, Show, Read)

data Parameter = Param Type Identifier
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data AssignLhs
    = AssignToIdent Identifier
    | AssignToArrayElem ArrayElem
    | AssignToPair PairElem
  deriving (Eq, Ord, Show, Read)

data AssignRhs
    = AssignExp Expression
    | AssignArrayLit ArrayLiteral
    | AssignPair Expression Expression
    | AssignPairElem PairElem
    | AssignFunctionCall Identifier [ArgumentList]
  deriving (Eq, Ord, Show, Read)

data ArgumentList = ArgumentList Expression
  deriving (Eq, Ord, Show, Read)

data PairElem = PairFst Expression | PairSnd Expression
  deriving (Eq, Ord, Show, Read)

data Type
    = BaseType BaseType
    | ArrayType ArrayDeclarationLiteral
    | PairType PairElemType PairElemType
  deriving (Eq, Ord, Show, Read)

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Ord, Show, Read)

data ArrayDeclarationLiteral = ArrayDeclarationLiteral Type
  deriving (Eq, Ord, Show, Read)

data ArrayElem = ArrayElem Identifier [ArrayAccess]
  deriving (Eq, Ord, Show, Read)

data ArrayAccess = ArrayAccess Expression
  deriving (Eq, Ord, Show, Read)

data ArrayLiteral = ArrayLiteral [ArrayLiteralElem]
  deriving (Eq, Ord, Show, Read)

data ArrayLiteralElem = ArrayLiteralElem Expression
  deriving (Eq, Ord, Show, Read)

data PairElemType
    = PairElemTypeBase BaseType
    | PairElemTypeArray ArrayDeclarationLiteral
    | PairElemTypePair
  deriving (Eq, Ord, Show, Read)

data Expression
    = IntExp IntLiteral
    | BoolExp BoolLiteral
    | CharExpr CharLiteral
    | StringExpr StringLiteral
    | PairExpr PairLiteral
    | IdentExpr Identifier
    | ArrayExpr ArrayElem
    | UExpr UnaryOperator Expression
    | BExp Expression BinaryOperator Expression
    | BracketExp Expression
  deriving (Eq, Ord, Show, Read)

data UnaryOperator
    = UBang | UMinus MinusLiteral | ULenght | UOrd | UChr
  deriving (Eq, Ord, Show, Read)

data BinaryOperator
    = BTimes
    | BDivide
    | BModulus
    | BPlus PlusLiteral
    | BMinus MinusLiteral
    | BMore
    | BLess
    | BMoreEqual
    | BLessEqual
    | BEqual
    | BNotEqual
    | BAnd
    | BOr
  deriving (Eq, Ord, Show, Read)

data IntLiteral
    = IntPlus PlusLiteral IntDigit
    | IntMinus MinusLiteral IntDigit
    | IntLiteral IntDigit
  deriving (Eq, Ord, Show, Read)