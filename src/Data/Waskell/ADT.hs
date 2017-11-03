module Data.Waskell.ADT where

--import qualified Data.Waskell.ADTHappy as H
import qualified Data.HashMap.Lazy as M

type ScopeBlock = ([Statement], NewScope)
type Position = (Int, Int)

type Scope = M.HashMap String Type

newtype NewScope = NewScope Scope
  deriving (Eq, Show)

newtype IntDigit = IntDigit (Position,String)
  deriving (Eq, Show)
newtype PlusLiteral = PlusLiteral (Position,String)
  deriving (Eq, Show)
newtype MinusLiteral = MinusLiteral (Position,String)
  deriving (Eq, Show)
newtype BoolLiteral = BoolLiteral (Position,String)
  deriving (Eq, Show)
newtype CharLiteral = CharLiteral (Position,String)
  deriving (Eq, Show)
newtype PairLiteral = PairLiteral (Position,String)
  deriving (Eq, Show)
newtype Identifier = Identifier (Position,String)
  deriving (Eq, Show)
newtype StringLiteral = StringLiteral (Position,String)
  deriving (Eq, Show)
data WaccTree = WaccTree Program
  deriving (Eq, Show)

data Program = Program [Function] ScopeBlock Position
  deriving (Eq, Show)

data Function = Function Type Identifier [Parameter] ScopeBlock Position
  deriving (Eq, Show)

data Parameter = Param Type Identifier Position
  deriving (Eq, Show)

data Statement
    = StatSkip Position
    | StatDecAss Type Identifier AssignRhs Position
    | StatAss AssignLhs AssignRhs Position
    | StatRead AssignLhs Position
    | StatFree Expression Position
    | StatReturn Expression Position
    | StatExit Expression Position
    | StatPrint Expression Position
    | StatPrintLn Expression Position
    | StatIf Expression ScopeBlock ScopeBlock Position
    | StatWhile Expression ScopeBlock Position
    | StatScope ScopeBlock Position
  deriving (Eq, Show)

data AssignLhs
    = AssignToIdent Identifier Position
    | AssignToArrayElem ArrayElem Position
    | AssignToPair PairElem Position
  deriving (Eq, Show)

data AssignRhs
    = AssignExp Expression Position
    | AssignArrayLit ArrayLiteral Position
    | AssignPair Expression Expression Position
    | AssignPairElem PairElem Position
    | AssignFunctionCall Identifier [ArgumentList] Position
  deriving (Eq, Show)

data ArgumentList = ArgumentList Expression Position
  deriving (Eq, Show)

data PairElem = PairFst Expression Position | PairSnd Expression Position
  deriving (Eq, Show)

data Type
    = BaseType BaseType
    | ArrayType ArrayDeclarationLiteral
    | PairType
  deriving (Eq, Show)

data BaseType = IntType | BoolType | CharType | StringType
  deriving (Eq, Show)

data ArrayDeclarationLiteral = ArrayDeclarationLiteral Type Position
  deriving (Eq, Show)

data ArrayElem = ArrayElem Identifier [ArrayAccess] Position
  deriving (Eq, Show)

data ArrayAccess = ArrayAccess Expression Position
  deriving (Eq, Show)

data ArrayLiteral = ArrayLiteral [ArrayLiteralElem] Position
  deriving (Eq, Show)

data ArrayLiteralElem = ArrayLiteralElem Expression Position
  deriving (Eq, Show)

data PairElemType
    = PairElemTypeBase BaseType Position
    | PairElemTypeArray ArrayDeclarationLiteral Position
    | PairElemTypePair Position
  deriving (Eq, Show)

data Expression
    = IntExp IntLiteral Position
    | BoolExp BoolLiteral Position
    | CharExpr CharLiteral Position
    | StringExpr StringLiteral Position
    | PairExpr PairLiteral Position
    | IdentExpr Identifier Position
    | ArrayExpr ArrayElem Position
    | UExpr UnaryOperator Expression Position
    | BExp Expression BinaryOperator Expression Position
    | BracketExp Expression Position
  deriving (Eq, Show)

data UnaryOperator
    = UBang Position | UMinus MinusLiteral Position | ULenght Position | UOrd Position | UChr Position
  deriving (Eq, Show)

data BinaryOperator
    = BTimes Position
    | BDivide Position
    | BModulus Position
    | BPlus PlusLiteral Position
    | BMinus MinusLiteral Position
    | BMore Position
    | BLess Position
    | BMoreEqual Position
    | BLessEqual Position
    | BEqual Position
    | BNotEqual Position
    | BAnd Position
    | BOr Position
  deriving (Eq, Show)

data IntLiteral
    = IntPlus PlusLiteral IntDigit Position
    | IntMinus MinusLiteral IntDigit Position
    | IntLiteral IntDigit Position
  deriving (Eq, Show)
