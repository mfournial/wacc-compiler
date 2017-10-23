import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.SmallCheck TODO uncomment when have small checks

-- import Data.Waskell TODO uncomment when have replaced undef by list of tokens

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testCase "Test for test" (succ 42 @?= (43 :: Int))
  -- , testGroup "SmallCheck" scTests
  -- , testGroup "Unit Tests" huTests
  ]

scTests :: [TestTree]
scTests = undefined -- TODO Write some tests for functions inside the class

huTests :: [TestTree]
huTests =
  [ testGroup "Valid wacc files" validTests
  , testGroup "Invalid wacc files" invalidTests
  ]

validTests :: [TestTree]
validTests =
  [ testGroup "Advanced" advanceTests  
  , testGroup "Array" arrayTests 
  , testGroup "Basic" basicTests
  , testGroup "Expression" expressionsTests
  , testGroup "Functions" functionsTests
  , testGroup "If" ifTests
  , testGroup "IO" ioTests
  , testGroup "Pairs" pairsTests
  , testGroup "RuntimeErr" runtimeErrTests
  , testGroup "Scope" scopeTests
  , testGroup "Sequence" sequenceTests
  , testGroup "Variable" variablesTests
  , testGroup "W" whileTests
  ] 

invalidTests :: [TestTree]
invalidTests =
  [ testGroup "Syntactic errors" syntacticTests
  , testGroup "Semantic errors"  semanticTests
  ]

advanceTests  :: [TestTree]
advanceTests =
  [ testCase "Binary Sort Tree" binarySortTree
  , testCase "Tic Tac Toe" ticTacToe
  , testCase "hash table" hashTable
  ]

binarySortTree :: Assertion
binarySortTree = undefined

ticTacToe :: Assertion
ticTacToe = undefined

hashTable :: Assertion
hashTable = undefined

arrayTests :: [TestTree]
arrayTests =
  [ testCase "Array" array
  , testCase "Array Basic" arrayBasic
  , testCase "Array Empty" arrayEmpty
  , testCase "Array Length" arrayLength
  , testCase "Array Lookup" arrayLookup
  , testCase "Array Nested" arrayNested
  , testCase "Array Print" arrayPrint
  , testCase "Array Simple" arraySimple
  , testCase "Modify String" modifyString
  , testCase "Print Ref" printRef
  ]

array :: Assertion
array = undefined

arrayBasic :: Assertion
arrayBasic = undefined

arrayEmpty :: Assertion
arrayEmpty = undefined

arrayLength :: Assertion
arrayLength = undefined

arrayLookup :: Assertion
arrayLookup = undefined

arrayNested :: Assertion
arrayNested = undefined

arrayPrint :: Assertion
arrayPrint = undefined

arraySimple :: Assertion
arraySimple = undefined

modifyString :: Assertion
modifyString = undefined

printRef :: Assertion
printRef = undefined

basicTests :: [TestTree]
basicTests =
  [ testGroup "Exit statements" exitTests
  , testGroup "Skip statements" skipTests
  ]

exitTests :: [TestTree]
exitTests =
  [ testCase "Exit-1" exit1
  , testCase "Exit Basic" exitBasic
  , testCase "Exit Basic 2" exitBasic2
  , testCase "Exit Wrap" exitWrap
  ]

exit1 :: Assertion
exit1 = undefined

exitBasic :: Assertion
exitBasic = undefined

exitBasic2 :: Assertion
exitBasic2 = undefined

exitWrap :: Assertion
exitWrap = undefined

skipTests :: [TestTree]
skipTests =
  [ testCase "Comment" comment
  , testCase "Comment In Line" commentInLine
  , testCase "Skip" skip
  ]

comment :: Assertion
comment = undefined

commentInLine :: Assertion
commentInLine = undefined

skip :: Assertion
skip = undefined

expressionsTests :: [TestTree]
expressionsTests =
  [ testCase "And Expression" andExpr
  , testCase "Bool Calc" boolCalc
  , testCase "Bool Expression" boolExpr1
  , testCase "Char Comparison" charComparisonExpr
  , testCase "Div Expression" divExpr
  , testCase "Equals Expression" equalsExpr
  , testCase "Greater Equals Expression" greaterEqExpr
  , testCase "Greater Expression" greaterExpr
  , testCase "Int Calc" intCalc
  , testCase "Int Expression" intExpr1
  , testCase "Less Char Expression" lessCharExpr
  , testCase "Less Quals Expression" lessEqExpr
  , testCase "Less Expression" lessExpr
  , testCase "Long Expression" longExpr
  , testCase "Long Expression 2" longExpr2
  , testCase "Long Expression 3" longExpr3
  , testCase "Long Split Expression" longSplitExpr
  , testCase "Long Split Expression 2" longSplitExpr2
  , testCase "Minus Expression" minusExpr
  , testCase "Minus minus Expression" minusMinusExpr
  , testCase "Minus No Whitespace" minusNoWhitespaceExpr
  , testCase "Minus Plus Expression" minusPlusExpr
  , testCase "Modulus Expression" modExpr
  , testCase "Mult Expression" multExpr
  , testCase "Mult No whitespace Expression" multNoWhitespaceExpr
  , testCase "Neg Both Division" negBothDiv
  , testCase "Neg Both Mod" negBothMod
  , testCase "Neg Dividend Division" negDividendDiv
  , testCase "Neg Dividend Modulus" negDividendMod
  , testCase "Neg Divisor Div" negDivisorDiv
  , testCase "Neg Divisor Mod" negDivisorMod
  , testCase "Neg Expression" negExpr
  , testCase "Not Expression" notExpr
  , testCase "Not Equal Expression" notequalsExpr
  , testCase "Or Expression" orExpr
  , testCase "Or and Chr Expression" ordAndchrExpr
  , testCase "Plus Expression" plusExpr
  , testCase "Plus Minus Expression" plusMinusExpr
  , testCase "Plus No Whitespace Expression" plusNoWhitespaceExpr
  , testCase "Plus Plus Expression" plusPlusExpr
  , testCase "Sequential Cound" sequentialCount
  , testCase "String Equals Expression" stringEqualsExpr
  ]

andExpr :: Assertion
andExpr = undefined

boolCalc :: Assertion
boolCalc = undefined

boolExpr1 :: Assertion
boolExpr1 = undefined

charComparisonExpr :: Assertion
charComparisonExpr = undefined

divExpr :: Assertion
divExpr = undefined

equalsExpr :: Assertion
equalsExpr = undefined

greaterEqExpr :: Assertion
greaterEqExpr = undefined

greaterExpr :: Assertion
greaterExpr = undefined

intCalc :: Assertion
intCalc = undefined

intExpr1 :: Assertion
intExpr1 = undefined

lessCharExpr :: Assertion
lessCharExpr = undefined

lessEqExpr :: Assertion
lessEqExpr = undefined

lessExpr :: Assertion
lessExpr = undefined

longExpr :: Assertion
longExpr = undefined

longExpr2 :: Assertion
longExpr2 = undefined

longExpr3 :: Assertion
longExpr3 = undefined

longSplitExpr :: Assertion
longSplitExpr = undefined

longSplitExpr2 :: Assertion
longSplitExpr2 = undefined

minusExpr :: Assertion
minusExpr = undefined

minusMinusExpr :: Assertion
minusMinusExpr = undefined

minusNoWhitespaceExpr :: Assertion
minusNoWhitespaceExpr = undefined

minusPlusExpr :: Assertion
minusPlusExpr = undefined

modExpr :: Assertion
modExpr = undefined

multExpr :: Assertion
multExpr = undefined

multNoWhitespaceExpr :: Assertion
multNoWhitespaceExpr = undefined

negBothDiv :: Assertion
negBothDiv = undefined

negBothMod :: Assertion
negBothMod = undefined

negDividendDiv :: Assertion
negDividendDiv = undefined

negDividendMod :: Assertion
negDividendMod = undefined

negDivisorDiv :: Assertion
negDivisorDiv = undefined

negDivisorMod :: Assertion
negDivisorMod = undefined

negExpr :: Assertion
negExpr = undefined

notExpr :: Assertion
notExpr = undefined

notequalsExpr :: Assertion
notequalsExpr = undefined

orExpr :: Assertion
orExpr = undefined

ordAndchrExpr :: Assertion
ordAndchrExpr = undefined

plusExpr :: Assertion
plusExpr = undefined

plusMinusExpr :: Assertion
plusMinusExpr = undefined

plusNoWhitespaceExpr :: Assertion
plusNoWhitespaceExpr = undefined

plusPlusExpr :: Assertion
plusPlusExpr = undefined

sequentialCount :: Assertion
sequentialCount = undefined

stringEqualsExpr :: Assertion
stringEqualsExpr = undefined

functionsTests :: [TestTree]
functionsTests =
  [ testGroup "Simple Functions Tests" simpleFunctionsTests
  , testGroup "Nested Functions Tests" nestedFunctionsTests
  ]

simpleFunctionsTests :: [TestTree]
simpleFunctionsTests = 
  [testCase "ASCII Table" asciiTable
  ,testCase "Function Declaration" functionDeclaration
  ,testCase "Function Many Arguments" functionManyArguments
  ,testCase "Function Return Pair" functionReturnPair
  ,testCase "Function Simple" functionSimple
  ,testCase "Function Update Parameter" functionUpdateParameter
  ,testCase "Inc Function" incFunction
  ,testCase "Neg Function" negFunction
  ,testCase "Same Arg Name" sameArgName
  ,testCase "Same Arg Name 2" sameArgName2
  ]

asciiTable :: Assertion
asciiTable = undefined

functionDeclaration :: Assertion
functionDeclaration = undefined

functionManyArguments :: Assertion
functionManyArguments = undefined

functionReturnPair :: Assertion
functionReturnPair = undefined

functionSimple :: Assertion
functionSimple = undefined

functionUpdateParameter :: Assertion
functionUpdateParameter = undefined

incFunction :: Assertion
incFunction = undefined

negFunction :: Assertion
negFunction = undefined

sameArgName :: Assertion
sameArgName = undefined

sameArgName2 :: Assertion
sameArgName2 = undefined

nestedFunctionsTests :: [TestTree]
nestedFunctionsTests =
  [ testCase "Fibonacci Full Rec" fibonacciFullRec
  , testCase "Fibonacci Recursive" fibonacciRecursive
  , testCase "Fixed Point Real Arithmetic" fixedPointRealArithmetic
  , testCase "Function Conditional Return" functionConditionalReturn
  ]

fibonacciFullRec :: Assertion
fibonacciFullRec = undefined

fibonacciRecursive :: Assertion
fibonacciRecursive = undefined

fixedPointRealArithmetic :: Assertion
fixedPointRealArithmetic = undefined

functionConditionalReturn :: Assertion
functionConditionalReturn = undefined

ifTests :: [TestTree]
ifTests =
  [ testCase "if 1" if1
  , testCase "If 2" if2
  , testCase "If 3" if3
  , testCase "If 4" if4
  , testCase "If 5" if5
  , testCase "If 6" if6
  , testCase "If Basic" ifBasic
  , testCase "If False" ifFalse
  , testCase "If True" ifTrue
  , testCase "Whitespace" whitespaceIf
  ]

if1 :: Assertion
if1 = undefined

if2 :: Assertion
if2 = undefined

if3 :: Assertion
if3 = undefined

if4 :: Assertion
if4 = undefined

if5 :: Assertion
if5 = undefined

if6 :: Assertion
if6 = undefined

ifBasic :: Assertion
ifBasic = undefined

ifFalse :: Assertion
ifFalse = undefined

ifTrue :: Assertion
ifTrue = undefined

whitespaceIf :: Assertion
whitespaceIf = undefined
     
ioTests :: [TestTree]
ioTests =
  [ testCase "IO Loop" ioLoop
  , testCase "IO Sequence" ioSequence
  , testGroup "Print Tests" printTests
  , testGroup "Read Tests" readTests
  ]

ioLoop :: Assertion
ioLoop = undefined

ioSequence :: Assertion
ioSequence = undefined

printTests :: [TestTree]
printTests = 
  [ testCase "Print" printT
  , testCase "Print Bool" printBool
  , testCase "Print Char" printChar
  , testCase "Print Esc Char" printEscChar
  , testCase "Print Int" printInt
  , testCase "Print Ln" println
  ]

printT :: Assertion
printT = undefined

printBool :: Assertion
printBool = undefined

printChar :: Assertion
printChar = undefined

printEscChar :: Assertion
printEscChar = undefined

printInt :: Assertion
printInt = undefined

println :: Assertion
println = undefined


readTests :: [TestTree]
readTests =
  [ testCase "Echo Big Int" echoBigInt
  , testCase "Echo Big Neg Int" echoBigNegInt
  , testCase "Echo Char" echoChar
  , testCase "Echo Int" echoInt
  , testCase "Echo Neg Int" echoNegInt
  , testCase "Echo Punc Char" echoPuncChar
  , testCase "Read" readT
  ]

echoBigInt :: Assertion
echoBigInt = undefined

echoBigNegInt :: Assertion
echoBigNegInt = undefined

echoChar :: Assertion
echoChar = undefined

echoInt :: Assertion
echoInt = undefined

echoNegInt :: Assertion
echoNegInt = undefined

echoPuncChar :: Assertion
echoPuncChar = undefined

readT :: Assertion
readT = undefined

pairsTests :: [TestTree]
pairsTests =
  [ testCase "Check Ref Pair" checkRefPair
  , testCase "Create Pair" createPair
  , testCase "Create Pair 2" createPair02
  , testCase "Create Pair 3" createPair03
  , testCase "Create Ref pair" createRefPair
  , testCase "Free pair" freePair
  , testCase "Create Pair 6" linkedList
  , testCase "Nested Pair" nestedPair
  , testCase "Null Pair" nullPair
  , testCase "Print Null" printNullP
  , testCase "Print Null Pair" printNullPair
  , testCase "Print Pair" printPair
  , testCase "Print Pair of Nulls" printPairOfNulls
  , testCase "Read Pair" readPair
  , testCase "Write Fst" writeFst
  , testCase "Write Snd" writeSnd
  ]

checkRefPair :: Assertion
checkRefPair = undefined

createPair :: Assertion
createPair = undefined

createPair02 :: Assertion
createPair02 = undefined

createPair03 :: Assertion
createPair03 = undefined

createRefPair :: Assertion
createRefPair = undefined

freePair :: Assertion
freePair = undefined

linkedList :: Assertion
linkedList = undefined

nestedPair :: Assertion
nestedPair = undefined

nullPair :: Assertion
nullPair = undefined

printNullP :: Assertion
printNullP = undefined

printNullPair :: Assertion
printNullPair = undefined

printPair :: Assertion
printPair = undefined

printPairOfNulls :: Assertion
printPairOfNulls = undefined

readPair :: Assertion
readPair = undefined

writeFst :: Assertion
writeFst = undefined

writeSnd :: Assertion
writeSnd = undefined

     
runtimeErrTests :: [TestTree]
runtimeErrTests = undefined -- TODO a looot

scopeTests :: [TestTree]
scopeTests =
  [ testCase "If Nested 1" ifNested1
  , testCase "If Nested 2" ifNested2
  , testCase "Indentation Not Important" indentationNotImportant
  , testCase "Ints and keywords" intsAndKeywords
  , testCase "Print All types" printAllTypes
  , testCase "Scope" scope
  , testCase "Scope Basic" scopeBasic
  , testCase "Scope Redefine" scopeRedefine
  , testCase "Scoped Simple Redefined" scopeSimpleRedefine
  , testCase "Scope Vars" scopeVars
  ]

ifNested1 :: Assertion
ifNested1 = undefined

ifNested2 :: Assertion
ifNested2 = undefined

indentationNotImportant :: Assertion
indentationNotImportant = undefined

intsAndKeywords :: Assertion
intsAndKeywords = undefined

printAllTypes :: Assertion
printAllTypes = undefined

scope :: Assertion
scope = undefined

scopeBasic :: Assertion
scopeBasic = undefined

scopeRedefine :: Assertion
scopeRedefine = undefined

scopeSimpleRedefine :: Assertion
scopeSimpleRedefine = undefined

scopeVars :: Assertion
scopeVars = undefined

sequenceTests :: [TestTree]
sequenceTests =
  [ testCase "Basic Seq" basicSeq
  , testCase "Basic Seq 2" basicSeq2
  , testCase "Bool Assignment" boolAssignment
  , testCase "Char Assignment" charAssignment
  , testCase "Exit Simple" exitSimple
  , testCase "Int Assignment" intAssignment
  , testCase "Int Leading Zeros" intLeadingZeros
  , testCase "String Assignment" stringAssignment
  ]

basicSeq :: Assertion
basicSeq = undefined

basicSeq2 :: Assertion
basicSeq2 = undefined

boolAssignment :: Assertion
boolAssignment = undefined

charAssignment :: Assertion
charAssignment = undefined

exitSimple :: Assertion
exitSimple = undefined

intAssignment :: Assertion
intAssignment = undefined

intLeadingZeros :: Assertion
intLeadingZeros = undefined

stringAssignment :: Assertion
stringAssignment = undefined

variablesTests :: [TestTree]
variablesTests =
  [ testCase "Underscore Variable Name" underscoreVarName
  , testCase "Bool Declaration" boolDeclaration
  , testCase "Bool Declaration 2" boolDeclaration2
  , testCase "Cap Char Declaration" capCharDeclaration
  , testCase "Char Declaration" charDeclaration
  , testCase "Char Declaration 2" charDeclaration2
  , testCase "Empty String Declaration" emptyStringDeclaration
  , testCase "Int Declaration" intDeclaration
  , testCase "Long Var Name" longVarNames
  , testCase "Many variables" manyVariables
  , testCase "Neg Int Declaration" negIntDeclaration
  , testCase "Punc Char Declaration" puncCharDeclaration
  , testCase "String declaration" stringDeclaration
  , testCase "Zero int Declaration" zeroIntDeclaration
  ]

underscoreVarName :: Assertion
underscoreVarName = undefined

boolDeclaration :: Assertion
boolDeclaration = undefined

boolDeclaration2 :: Assertion
boolDeclaration2 = undefined

capCharDeclaration :: Assertion
capCharDeclaration = undefined

charDeclaration :: Assertion
charDeclaration = undefined

charDeclaration2 :: Assertion
charDeclaration2 = undefined

emptyStringDeclaration :: Assertion
emptyStringDeclaration = undefined

intDeclaration :: Assertion
intDeclaration = undefined

longVarNames :: Assertion
longVarNames = undefined

manyVariables :: Assertion
manyVariables = undefined

negIntDeclaration :: Assertion
negIntDeclaration = undefined

puncCharDeclaration :: Assertion
puncCharDeclaration = undefined

stringDeclaration :: Assertion
stringDeclaration = undefined

zeroIntDeclaration :: Assertion
zeroIntDeclaration = undefined

whileTests :: [TestTree]
whileTests =
  [ testCase "Fibonacci Full Iterative" fibonacciFullIt
  , testCase "Fibonacci Iterative" fibonacciIterative
  , testCase "Loop Char Condition" loopCharCondition
  , testCase "Loop Int Condition" loopIntCondition
  , testCase "Max" maxT
  , testCase "Min" minT
  , testCase "Max" rmStyleAdd
  , testCase "Remove Style Add IO" rmStyleAddIO
  , testCase "While Basic" whileBasic
  , testCase "While Bool Flip" whileBoolFlip
  , testCase "While Count" whileCount
  , testCase "While False" whileFalse
  ]

fibonacciFullIt :: Assertion
fibonacciFullIt = undefined

fibonacciIterative :: Assertion
fibonacciIterative = undefined

loopCharCondition :: Assertion
loopCharCondition = undefined

loopIntCondition :: Assertion
loopIntCondition = undefined

maxT :: Assertion
maxT = undefined

minT :: Assertion
minT = undefined

rmStyleAdd :: Assertion
rmStyleAdd = undefined

rmStyleAddIO :: Assertion
rmStyleAddIO = undefined

whileBasic :: Assertion
whileBasic = undefined

whileBoolFlip :: Assertion
whileBoolFlip = undefined

whileCount :: Assertion
whileCount = undefined

whileFalse :: Assertion
whileFalse = undefined

syntacticTests :: [TestTree]
syntacticTests = undefined

semanticTests :: [TestTree]
semanticTests = undefined
