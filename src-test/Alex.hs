import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe (unsafePerformIO)

import Alex.Waskell
import Data.Waskell.Error

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

strip :: ErrorList [Token] -> [Tok]
strip (ErrorList (Just a) []) = map stip' a
  where
    stip' (PT _ t) = t
strip (ErrorList _ _) = []

tests :: [TestTree]
tests =
  [testGroup "Lexer Tests" huTests]

huTests :: [TestTree]
huTests =
  [ testGroup "Valid wacc files" validTests
  --, testGroup "Invalid wacc files" invalidTests
  ]

validTests :: [TestTree]
validTests =
  [ testGroup "Advanced" advanceTests  
  , testGroup "Arrays" arrayTests 
  , testGroup "Basic" basicTests
  , testGroup "Expressions" expressionsTests
  , testGroup "Functions" functionsTests
  , testGroup "If" ifTests
  , testGroup "IO" ioTests
  , testGroup "Pairs" pairsTests
  , testGroup "RuntimeErr" runtimeErrTests
  , testGroup "Scope" scopeTests
  , testGroup "Sequences" sequenceTests
  , testGroup "Variables" variablesTests
  , testGroup "While" whileTests
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
binarySortTree = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/advanced/binarySortTree.wacc"))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "createNewNode"), T_LParenT, T_IntT, (T_Identifier "value"), T_CoT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "left"), T_CoT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "right"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "left"), T_CoT, (T_Identifier "right"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "q"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "value"), T_CoT, (T_Identifier "p"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "q"), T_EndT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "insert"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_CoT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IfT, (T_Identifier "root"), T_EqT, T_NullT, T_ThenT, (T_Identifier "root"), T_EqualT, T_CallT, (T_Identifier "createNewNode"), T_LParenT, (T_Identifier "n"), T_CoT, T_NullT, T_CoT, T_NullT, T_RParenT, T_ElseT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_SndT, (T_Identifier "root"), T_SepT, T_IntT, (T_Identifier "current"), T_EqualT, T_FstT, (T_Identifier "root"), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "q"), T_EqualT, T_NullT, T_SepT, T_IfT, (T_Identifier "n"), T_LessT, (T_Identifier "current"), T_ThenT, (T_Identifier "q"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_FstT, (T_Identifier "p"), T_EqualT, T_CallT, (T_Identifier "insert"), T_LParenT, (T_Identifier "q"), T_CoT, (T_Identifier "n"), T_RParenT, T_ElseT, (T_Identifier "q"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_SndT, (T_Identifier "p"), T_EqualT, T_CallT, (T_Identifier "insert"), T_LParenT, (T_Identifier "q"), T_CoT, (T_Identifier "n"), T_RParenT, T_FiT, T_FiT, T_SepT, T_ReturnT, (T_Identifier "root"), T_EndT, T_IntT, (T_Identifier "printTree"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_RParenT, T_IsT, T_IfT, (T_Identifier "root"), T_EqT, T_NullT, T_ThenT, T_ReturnT, (T_IntDigit "0"), T_ElseT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "body"), T_EqualT, T_SndT, (T_Identifier "root"), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_FstT, (T_Identifier "body"), T_SepT, T_IntT, (T_Identifier "temp"), T_EqualT, T_CallT, (T_Identifier "printTree"), T_LParenT, (T_Identifier "p"), T_RParenT, T_SepT, (T_Identifier "temp"), T_EqualT, T_FstT, (T_Identifier "root"), T_SepT, T_PrintT, (T_Identifier "temp"), T_SepT, T_PrintT, (T_CharLiteral "' '"), T_SepT, (T_Identifier "p"), T_EqualT, T_SndT, (T_Identifier "body"), T_SepT, (T_Identifier "temp"), T_EqualT, T_CallT, (T_Identifier "printTree"), T_LParenT, (T_Identifier "p"), T_RParenT, T_SepT, T_ReturnT, (T_IntDigit "0"), T_FiT, T_EndT, T_IntT, (T_Identifier "n"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_StringLiteral "\"Please enter the number of integers to insert: \""), T_SepT, T_ReadT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\"There are \""), T_SepT, T_PrintT, (T_Identifier "n"), T_SepT, T_PrintLnT, (T_StringLiteral "\" integers.\""), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_EqualT, T_NullT, T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "n"), T_DoT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_StringLiteral "\"Please enter the number at position \""), T_SepT, T_PrintT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintT, (T_StringLiteral "\" : \""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, (T_Identifier "root"), T_EqualT, T_CallT, (T_Identifier "insert"), T_LParenT, (T_Identifier "root"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintT, (T_StringLiteral "\"Here are the numbers sorted: \""), T_SepT, (T_Identifier "i"), T_EqualT, T_CallT, (T_Identifier "printTree"), T_LParenT, (T_Identifier "root"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_EndT] 

ticTacToe :: Assertion
ticTacToe = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/advanced/ticTacToe.wacc"))
  @=? [ T_BeginT, T_CharT, (T_Identifier "chooseSymbol"), T_LParenT, T_RParenT, T_IsT, T_PrintLnT, (T_StringLiteral "\"========= Tic Tac Toe ================\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=  Because we know you want to win   =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"======================================\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=                                    =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= Who would you like to be?          =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=   x  (play first)                  =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=   o  (play second)                 =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=   q  (quit)                        =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=                                    =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"======================================\""), T_SepT, T_CharT, (T_Identifier "chosen"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_WhileT, (T_Identifier "chosen"), T_EqT, (T_CharLiteral "'\\0'"), T_DoT, T_PrintT, (T_StringLiteral "\"Which symbol you would like to choose: \""), T_SepT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_ReadT, (T_Identifier "c"), T_SepT, T_IfT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'x'"), T_OrT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'X'"), T_ThenT, (T_Identifier "chosen"), T_EqualT, (T_CharLiteral "'x'"), T_ElseT, T_IfT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'o'"), T_OrT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'O'"), T_ThenT, (T_Identifier "chosen"), T_EqualT, (T_CharLiteral "'o'"), T_ElseT, T_IfT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'q'"), T_OrT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'Q'"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"Goodbye safety.\""), T_SepT, T_ExitT, (T_IntDigit "0"), T_ElseT, T_PrintT, (T_StringLiteral "\"Invalid symbol: \""), T_SepT, T_PrintLnT, (T_Identifier "c"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Please try again.\""), T_FiT, T_FiT, T_FiT, T_DoneT, T_SepT, T_PrintT, (T_StringLiteral "\"You have chosen: \""), T_SepT, T_PrintLnT, (T_Identifier "chosen"), T_SepT, T_ReturnT, (T_Identifier "chosen"), T_EndT, T_BoolT, (T_Identifier "printBoard"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "board"), T_SepT, T_PrintLnT, (T_StringLiteral "\" 1 2 3\""), T_SepT, T_PrintT, (T_StringLiteral "\"1\""), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\" -+-+-\""), T_SepT, T_PrintT, (T_StringLiteral "\"2\""), T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\" -+-+-\""), T_SepT, T_PrintT, (T_StringLiteral "\"3\""), T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "printRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "row"), T_SepT, T_CharT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "row"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printCell"), T_LParenT, (T_Identifier "cell1"), T_RParenT, T_SepT, T_PrintT, (T_CharLiteral "'|'"), T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printCell"), T_LParenT, (T_Identifier "cell2"), T_RParenT, T_SepT, T_PrintT, (T_CharLiteral "'|'"), T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printCell"), T_LParenT, (T_Identifier "cell3"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "printCell"), T_LParenT, T_CharT, (T_Identifier "cell"), T_RParenT, T_IsT, T_IfT, (T_Identifier "cell"), T_EqT, (T_CharLiteral "'\\0'"), T_ThenT, T_PrintT, (T_CharLiteral "' '"), T_ElseT, T_PrintT, (T_Identifier "cell"), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "askForAMoveHuman"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_BoolT, (T_Identifier "success"), T_EqualT, T_FalseToken, T_SepT, T_IntT, (T_Identifier "row"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "column"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, T_NotT, (T_Identifier "success"), T_DoT, T_PrintLnT, (T_StringLiteral "\"What is your next move?\""), T_SepT, T_PrintT, (T_StringLiteral "\" row (1-3): \""), T_SepT, T_ReadT, (T_Identifier "row"), T_SepT, T_PrintT, (T_StringLiteral "\" column (1-3): \""), T_SepT, T_ReadT, (T_Identifier "column"), T_SepT, (T_Identifier "success"), T_EqualT, T_CallT, (T_Identifier "validateMove"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "row"), T_CoT, (T_Identifier "column"), T_RParenT, T_SepT, T_IfT, (T_Identifier "success"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_Identifier "row"), T_SepT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_EqualT, (T_Identifier "column"), T_SepT, T_ReturnT, T_TrueToken, T_ElseT, T_PrintLnT, (T_StringLiteral "\"Your move is invalid. Please try again.\""), T_FiT, T_DoneT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "validateMove"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_IfT, (T_IntDigit "1"), T_LessEqT, (T_Identifier "moveRow"), T_AndT, (T_Identifier "moveRow"), T_LessEqT, (T_IntDigit "3"), T_AndT, (T_IntDigit "1"), T_LessEqT, (T_Identifier "moveColumn"), T_AndT, (T_Identifier "moveColumn"), T_LessEqT, (T_IntDigit "3"), T_ThenT, T_CharT, (T_Identifier "sym"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "moveRow"), T_CoT, (T_Identifier "moveColumn"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "sym"), T_EqT, (T_CharLiteral "'\\0'"), T_ElseT, T_ReturnT, T_FalseToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "notifyMoveHuman"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_CharT, (T_Identifier "playerSymbol"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_PrintT, (T_StringLiteral "\"The AI played at row \""), T_SepT, T_PrintT, (T_Identifier "moveRow"), T_SepT, T_PrintT, (T_StringLiteral "\" column \""), T_SepT, T_PrintLnT, (T_Identifier "moveColumn"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "initAI"), T_LParenT, T_CharT, (T_Identifier "aiSymbol"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_PairT, T_RParenT, (T_Identifier "info"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "aiSymbol"), T_CoT, T_NullT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_EqualT, T_CallT, (T_Identifier "generateAllPossibleStates"), T_LParenT, (T_Identifier "aiSymbol"), T_RParenT, T_SepT, T_IntT, (T_Identifier "value"), T_EqualT, T_CallT, (T_Identifier "setValuesForAllStates"), T_LParenT, (T_Identifier "stateTree"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_CharLiteral "'x'"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "info"), T_CoT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "aiData"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "generateAllPossibleStates"), T_LParenT, T_CharT, (T_Identifier "aiSymbol"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_CallT, (T_Identifier "allocateNewBoard"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "rootState"), T_EqualT, T_CallT, (T_Identifier "convertFromBoardToState"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, (T_Identifier "rootState"), T_EqualT, T_CallT, (T_Identifier "generateNextStates"), T_LParenT, (T_Identifier "rootState"), T_CoT, (T_CharLiteral "'x'"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "rootState"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "convertFromBoardToState"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_CallT, (T_Identifier "generateEmptyPointerBoard"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "pointers"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "front"), T_CoT, (T_IntDigit "0"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "state"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "generateEmptyPointerBoard"), T_LParenT, T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_CallT, (T_Identifier "generateEmptyPointerRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_CallT, (T_Identifier "generateEmptyPointerRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_CallT, (T_Identifier "generateEmptyPointerRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "row1"), T_CoT, (T_Identifier "row2"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "front"), T_CoT, (T_Identifier "row3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "root"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "generateEmptyPointerRow"), T_LParenT, T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_NewpairT, T_LParenT, T_NullT, T_CoT, T_NullT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "front"), T_CoT, T_NullT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "root"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "generateNextStates"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state"), T_CoT, T_CharT, (T_Identifier "currentPlayer"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "state"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "previousPlayer"), T_EqualT, T_CallT, (T_Identifier "oppositeSymbol"), T_LParenT, (T_Identifier "currentPlayer"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "won"), T_EqualT, T_CallT, (T_Identifier "hasWon"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "previousPlayer"), T_RParenT, T_SepT, T_IfT, (T_Identifier "won"), T_ThenT, T_ReturnT, (T_Identifier "state"), T_ElseT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesBoard"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "pointers"), T_CoT, (T_Identifier "currentPlayer"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "state"), T_FiT, T_EndT, T_BoolT, (T_Identifier "generateNextStatesBoard"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_CoT, T_CharT, (T_Identifier "currentPlayer"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "board"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "frontP"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1P"), T_EqualT, T_FstT, (T_Identifier "frontP"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2P"), T_EqualT, T_SndT, (T_Identifier "frontP"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3P"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesRow"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "row1"), T_CoT, (T_Identifier "row1P"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_IntDigit "1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesRow"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "row2"), T_CoT, (T_Identifier "row2P"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesRow"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "row3"), T_CoT, (T_Identifier "row3P"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "generateNextStatesRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointerRow"), T_CoT, T_CharT, (T_Identifier "currentPlayer"), T_CoT, T_IntT, (T_Identifier "rowNumber"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "row"), T_SepT, T_CharT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "row"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "frontP"), T_EqualT, T_FstT, (T_Identifier "pointerRow"), T_SepT, T_FstT, (T_Identifier "frontP"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesCell"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "cell1"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_Identifier "rowNumber"), T_CoT, (T_IntDigit "1"), T_RParenT, T_SepT, T_SndT, (T_Identifier "frontP"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesCell"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "cell2"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_Identifier "rowNumber"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, T_SndT, (T_Identifier "pointerRow"), T_EqualT, T_CallT, (T_Identifier "generateNextStatesCell"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "cell3"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_Identifier "rowNumber"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "generateNextStatesCell"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "cell"), T_CoT, T_CharT, (T_Identifier "currentPlayer"), T_CoT, T_IntT, (T_Identifier "rowNumber"), T_CoT, T_IntT, (T_Identifier "columnNumber"), T_RParenT, T_IsT, T_IfT, (T_Identifier "cell"), T_EqT, (T_CharLiteral "'\\0'"), T_ThenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board2"), T_EqualT, T_CallT, (T_Identifier "cloneBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "placeMove"), T_LParenT, (T_Identifier "board2"), T_CoT, (T_Identifier "currentPlayer"), T_CoT, (T_Identifier "rowNumber"), T_CoT, (T_Identifier "columnNumber"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state"), T_EqualT, T_CallT, (T_Identifier "convertFromBoardToState"), T_LParenT, (T_Identifier "board2"), T_RParenT, T_SepT, T_CharT, (T_Identifier "nextPlayer"), T_EqualT, T_CallT, (T_Identifier "oppositeSymbol"), T_LParenT, (T_Identifier "currentPlayer"), T_RParenT, T_SepT, (T_Identifier "state"), T_EqualT, T_CallT, (T_Identifier "generateNextStates"), T_LParenT, (T_Identifier "state"), T_CoT, (T_Identifier "nextPlayer"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "state"), T_ElseT, T_ReturnT, T_NullT, T_FiT, T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "cloneBoard"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board2"), T_EqualT, T_CallT, (T_Identifier "allocateNewBoard"), T_LParenT, T_RParenT, T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "copyBoard"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "board2"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "board2"), T_EndT, T_BoolT, (T_Identifier "copyBoard"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "from"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "to"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "frontFrom"), T_EqualT, T_FstT, (T_Identifier "from"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1From"), T_EqualT, T_FstT, (T_Identifier "frontFrom"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2From"), T_EqualT, T_SndT, (T_Identifier "frontFrom"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3From"), T_EqualT, T_SndT, (T_Identifier "from"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "frontTo"), T_EqualT, T_FstT, (T_Identifier "to"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1To"), T_EqualT, T_FstT, (T_Identifier "frontTo"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2To"), T_EqualT, T_SndT, (T_Identifier "frontTo"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3To"), T_EqualT, T_SndT, (T_Identifier "to"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "copyRow"), T_LParenT, (T_Identifier "row1From"), T_CoT, (T_Identifier "row1To"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "copyRow"), T_LParenT, (T_Identifier "row2From"), T_CoT, (T_Identifier "row2To"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "copyRow"), T_LParenT, (T_Identifier "row3From"), T_CoT, (T_Identifier "row3To"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "copyRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "from"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "to"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "frontFrom"), T_EqualT, T_FstT, (T_Identifier "from"), T_SepT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "frontTo"), T_EqualT, T_FstT, (T_Identifier "to"), T_SepT, T_FstT, (T_Identifier "frontTo"), T_EqualT, T_FstT, (T_Identifier "frontFrom"), T_SepT, T_SndT, (T_Identifier "frontTo"), T_EqualT, T_SndT, (T_Identifier "frontFrom"), T_SepT, T_SndT, (T_Identifier "to"), T_EqualT, T_SndT, (T_Identifier "from"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_IntT, (T_Identifier "setValuesForAllStates"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state"), T_CoT, T_CharT, (T_Identifier "aiSymbol"), T_CoT, T_CharT, (T_Identifier "currentPlayer"), T_RParenT, T_IsT, T_IntT, (T_Identifier "outValue"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IfT, (T_Identifier "state"), T_EqT, T_NullT, T_ThenT, T_IfT, (T_Identifier "currentPlayer"), T_EqT, (T_Identifier "aiSymbol"), T_ThenT, (T_Identifier "outValue"), T_EqualT, (T_IntDigit "101"), T_ElseT, (T_Identifier "outValue"), T_EqualT, T_MinusToken, (T_IntDigit "101"), T_FiT, T_ElseT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "state"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "anotherPlayer"), T_EqualT, T_CallT, (T_Identifier "oppositeSymbol"), T_LParenT, (T_Identifier "currentPlayer"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "won"), T_EqualT, T_CallT, (T_Identifier "hasWon"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "anotherPlayer"), T_RParenT, T_SepT, T_IfT, (T_Identifier "won"), T_ThenT, T_IfT, (T_Identifier "anotherPlayer"), T_EqT, (T_Identifier "aiSymbol"), T_ThenT, (T_Identifier "outValue"), T_EqualT, (T_IntDigit "100"), T_ElseT, (T_Identifier "outValue"), T_EqualT, T_MinusToken, (T_IntDigit "100"), T_FiT, T_ElseT, T_BoolT, (T_Identifier "hasEmptyCell"), T_EqualT, T_CallT, (T_Identifier "containEmptyCell"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, T_IfT, (T_Identifier "hasEmptyCell"), T_ThenT, (T_Identifier "outValue"), T_EqualT, T_CallT, (T_Identifier "calculateValuesFromNextStates"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "anotherPlayer"), T_RParenT, T_SepT, T_IfT, (T_Identifier "outValue"), T_EqT, (T_IntDigit "100"), T_ThenT, (T_Identifier "outValue"), T_EqualT, (T_IntDigit "90"), T_ElseT, T_SkipT, T_FiT, T_ElseT, (T_Identifier "outValue"), T_EqualT, (T_IntDigit "0"), T_FiT, T_FiT, T_SepT, T_SndT, (T_Identifier "state"), T_EqualT, (T_Identifier "outValue"), T_FiT, T_SepT, T_ReturnT, (T_Identifier "outValue"), T_EndT, T_IntT, (T_Identifier "calculateValuesFromNextStates"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_CoT, T_CharT, (T_Identifier "aiSymbol"), T_CoT, T_CharT, (T_Identifier "playerOfNextState"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_IntT, (T_Identifier "value1"), T_EqualT, T_CallT, (T_Identifier "calculateValuesFromNextStatesRow"), T_LParenT, (T_Identifier "row1"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "value2"), T_EqualT, T_CallT, (T_Identifier "calculateValuesFromNextStatesRow"), T_LParenT, (T_Identifier "row2"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "value3"), T_EqualT, T_CallT, (T_Identifier "calculateValuesFromNextStatesRow"), T_LParenT, (T_Identifier "row3"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "out"), T_EqualT, T_CallT, (T_Identifier "combineValue"), T_LParenT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_CoT, (T_Identifier "value1"), T_CoT, (T_Identifier "value2"), T_CoT, (T_Identifier "value3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "out"), T_EndT, T_IntT, (T_Identifier "calculateValuesFromNextStatesRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_CoT, T_CharT, (T_Identifier "aiSymbol"), T_CoT, T_CharT, (T_Identifier "playerOfNextState"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state3"), T_EqualT, T_SndT, (T_Identifier "rowPointers"), T_SepT, T_IntT, (T_Identifier "value1"), T_EqualT, T_CallT, (T_Identifier "setValuesForAllStates"), T_LParenT, (T_Identifier "state1"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "value2"), T_EqualT, T_CallT, (T_Identifier "setValuesForAllStates"), T_LParenT, (T_Identifier "state2"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "value3"), T_EqualT, T_CallT, (T_Identifier "setValuesForAllStates"), T_LParenT, (T_Identifier "state3"), T_CoT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_RParenT, T_SepT, T_IntT, (T_Identifier "out"), T_EqualT, T_CallT, (T_Identifier "combineValue"), T_LParenT, (T_Identifier "aiSymbol"), T_CoT, (T_Identifier "playerOfNextState"), T_CoT, (T_Identifier "value1"), T_CoT, (T_Identifier "value2"), T_CoT, (T_Identifier "value3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "out"), T_EndT, T_IntT, (T_Identifier "combineValue"), T_LParenT, T_CharT, (T_Identifier "aiSymbol"), T_CoT, T_CharT, (T_Identifier "playerOfNextState"), T_CoT, T_IntT, (T_Identifier "value1"), T_CoT, T_IntT, (T_Identifier "value2"), T_CoT, T_IntT, (T_Identifier "value3"), T_RParenT, T_IsT, T_IntT, (T_Identifier "out"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IfT, (T_Identifier "aiSymbol"), T_EqT, (T_Identifier "playerOfNextState"), T_ThenT, (T_Identifier "out"), T_EqualT, T_CallT, (T_Identifier "min3"), T_LParenT, (T_Identifier "value1"), T_CoT, (T_Identifier "value2"), T_CoT, (T_Identifier "value3"), T_RParenT, T_ElseT, (T_Identifier "out"), T_EqualT, T_CallT, (T_Identifier "max3"), T_LParenT, (T_Identifier "value1"), T_CoT, (T_Identifier "value2"), T_CoT, (T_Identifier "value3"), T_RParenT, T_FiT, T_SepT, T_ReturnT, (T_Identifier "out"), T_EndT, T_IntT, (T_Identifier "min3"), T_LParenT, T_IntT, (T_Identifier "a"), T_CoT, T_IntT, (T_Identifier "b"), T_CoT, T_IntT, (T_Identifier "c"), T_RParenT, T_IsT, T_IfT, (T_Identifier "a"), T_LessT, (T_Identifier "b"), T_ThenT, T_IfT, (T_Identifier "a"), T_LessT, (T_Identifier "c"), T_ThenT, T_ReturnT, (T_Identifier "a"), T_ElseT, T_ReturnT, (T_Identifier "c"), T_FiT, T_ElseT, T_IfT, (T_Identifier "b"), T_LessT, (T_Identifier "c"), T_ThenT, T_ReturnT, (T_Identifier "b"), T_ElseT, T_ReturnT, (T_Identifier "c"), T_FiT, T_FiT, T_EndT, T_IntT, (T_Identifier "max3"), T_LParenT, T_IntT, (T_Identifier "a"), T_CoT, T_IntT, (T_Identifier "b"), T_CoT, T_IntT, (T_Identifier "c"), T_RParenT, T_IsT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_Identifier "b"), T_ThenT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_Identifier "c"), T_ThenT, T_ReturnT, (T_Identifier "a"), T_ElseT, T_ReturnT, (T_Identifier "c"), T_FiT, T_ElseT, T_IfT, (T_Identifier "b"), T_GreaterT, (T_Identifier "c"), T_ThenT, T_ReturnT, (T_Identifier "b"), T_ElseT, T_ReturnT, (T_Identifier "c"), T_FiT, T_FiT, T_EndT, T_BoolT, (T_Identifier "destroyAI"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_PairT, T_RParenT, (T_Identifier "info"), T_EqualT, T_FstT, (T_Identifier "aiData"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_EqualT, T_SndT, (T_Identifier "aiData"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "info"), T_SepT, T_FreeT, (T_Identifier "aiData"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "askForAMoveAI"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_CharT, (T_Identifier "playerSymbol"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_PairT, T_RParenT, (T_Identifier "info"), T_EqualT, T_FstT, (T_Identifier "aiData"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_EqualT, T_SndT, (T_Identifier "aiData"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "stateTree"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_IntT, (T_Identifier "stateValue"), T_EqualT, T_SndT, (T_Identifier "stateTree"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "findTheBestMove"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_Identifier "stateValue"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"AI is cleaning up its memory...\""), T_SepT, T_SndT, (T_Identifier "aiData"), T_EqualT, T_CallT, (T_Identifier "deleteAllOtherChildren"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteThisStateOnly"), T_LParenT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "findTheBestMove"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_CoT, T_IntT, (T_Identifier "stateValue"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_IfT, (T_Identifier "stateValue"), T_EqT, (T_IntDigit "90"), T_ThenT, T_BoolT, (T_Identifier "found"), T_EqualT, T_CallT, (T_Identifier "findMoveWithGivenValue"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_IntDigit "100"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_IfT, (T_Identifier "found"), T_ThenT, T_ReturnT, T_TrueToken, T_ElseT, T_SkipT, T_FiT, T_ElseT, T_SkipT, T_FiT, T_SepT, T_BoolT, (T_Identifier "found"), T_EqualT, T_CallT, (T_Identifier "findMoveWithGivenValue"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_Identifier "stateValue"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_IfT, (T_Identifier "found"), T_ThenT, T_ReturnT, T_TrueToken, T_ElseT, T_PrintLnT, (T_StringLiteral "\"Internal Error: cannot find the next move for the AI\""), T_SepT, T_ExitT, T_MinusToken, (T_IntDigit "1"), T_FiT, T_EndT, T_BoolT, (T_Identifier "findMoveWithGivenValue"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_CoT, T_IntT, (T_Identifier "stateValue"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_BoolT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "findMoveWithGivenValueRow"), T_LParenT, (T_Identifier "row1"), T_CoT, (T_Identifier "stateValue"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_IntDigit "1"), T_ElseT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "findMoveWithGivenValueRow"), T_LParenT, (T_Identifier "row2"), T_CoT, (T_Identifier "stateValue"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_IntDigit "2"), T_ElseT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "findMoveWithGivenValueRow"), T_LParenT, (T_Identifier "row3"), T_CoT, (T_Identifier "stateValue"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_IntDigit "3"), T_ElseT, T_ReturnT, T_FalseToken, T_FiT, T_FiT, T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "findMoveWithGivenValueRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_CoT, T_IntT, (T_Identifier "stateValue"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "rowPointers"), T_SepT, T_BoolT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "hasGivenStateValue"), T_LParenT, (T_Identifier "cell1"), T_CoT, (T_Identifier "stateValue"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_EqualT, (T_IntDigit "1"), T_ElseT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "hasGivenStateValue"), T_LParenT, (T_Identifier "cell2"), T_CoT, (T_Identifier "stateValue"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_EqualT, (T_IntDigit "2"), T_ElseT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "hasGivenStateValue"), T_LParenT, (T_Identifier "cell3"), T_CoT, (T_Identifier "stateValue"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_EqualT, (T_IntDigit "3"), T_ElseT, T_ReturnT, T_FalseToken, T_FiT, T_FiT, T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "hasGivenStateValue"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "state"), T_CoT, T_IntT, (T_Identifier "stateValue"), T_RParenT, T_IsT, T_IfT, (T_Identifier "state"), T_EqT, T_NullT, T_ThenT, T_ReturnT, T_FalseToken, T_ElseT, T_IntT, (T_Identifier "actual"), T_EqualT, T_SndT, (T_Identifier "state"), T_SepT, T_ReturnT, (T_Identifier "actual"), T_EqT, (T_Identifier "stateValue"), T_FiT, T_EndT, T_BoolT, (T_Identifier "notifyMoveAI"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_CharT, (T_Identifier "playerSymbol"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_EqualT, T_SndT, (T_Identifier "aiData"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "stateTree"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PrintLnT, (T_StringLiteral "\"AI is cleaning up its memory...\""), T_SepT, T_SndT, (T_Identifier "aiData"), T_EqualT, T_CallT, (T_Identifier "deleteAllOtherChildren"), T_LParenT, (T_Identifier "pointers"), T_CoT, (T_Identifier "moveRow"), T_CoT, (T_Identifier "moveColumn"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteThisStateOnly"), T_LParenT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "deleteAllOtherChildren"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "toKeepRow"), T_EqualT, T_NullT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "toDeleteRow1"), T_EqualT, T_NullT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "toDeleteRow2"), T_EqualT, T_NullT, T_SepT, T_IfT, (T_Identifier "moveRow"), T_EqT, (T_IntDigit "1"), T_ThenT, (T_Identifier "toKeepRow"), T_EqualT, (T_Identifier "row1"), T_SepT, (T_Identifier "toDeleteRow1"), T_EqualT, (T_Identifier "row2"), T_SepT, (T_Identifier "toDeleteRow2"), T_EqualT, (T_Identifier "row3"), T_ElseT, (T_Identifier "toDeleteRow1"), T_EqualT, (T_Identifier "row1"), T_SepT, T_IfT, (T_Identifier "moveRow"), T_EqT, (T_IntDigit "2"), T_ThenT, (T_Identifier "toKeepRow"), T_EqualT, (T_Identifier "row2"), T_SepT, (T_Identifier "toDeleteRow2"), T_EqualT, (T_Identifier "row3"), T_ElseT, (T_Identifier "toKeepRow"), T_EqualT, (T_Identifier "row3"), T_SepT, (T_Identifier "toDeleteRow2"), T_EqualT, (T_Identifier "row2"), T_FiT, T_FiT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "out"), T_EqualT, T_CallT, (T_Identifier "deleteAllOtherChildrenRow"), T_LParenT, (T_Identifier "toKeepRow"), T_CoT, (T_Identifier "moveColumn"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, (T_Identifier "toDeleteRow1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, (T_Identifier "toDeleteRow2"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "out"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "deleteAllOtherChildrenRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "toKeepCell"), T_EqualT, T_NullT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "toDeleteCell1"), T_EqualT, T_NullT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "toDeleteCell2"), T_EqualT, T_NullT, T_SepT, T_IfT, (T_Identifier "moveColumn"), T_EqT, (T_IntDigit "1"), T_ThenT, (T_Identifier "toKeepCell"), T_EqualT, (T_Identifier "cell1"), T_SepT, (T_Identifier "toDeleteCell1"), T_EqualT, (T_Identifier "cell2"), T_SepT, (T_Identifier "toDeleteCell2"), T_EqualT, (T_Identifier "cell3"), T_ElseT, (T_Identifier "toDeleteCell1"), T_EqualT, (T_Identifier "cell1"), T_SepT, T_IfT, (T_Identifier "moveColumn"), T_EqT, (T_IntDigit "2"), T_ThenT, (T_Identifier "toKeepCell"), T_EqualT, (T_Identifier "cell2"), T_SepT, (T_Identifier "toDeleteCell2"), T_EqualT, (T_Identifier "cell3"), T_ElseT, (T_Identifier "toKeepCell"), T_EqualT, (T_Identifier "cell3"), T_SepT, (T_Identifier "toDeleteCell2"), T_EqualT, (T_Identifier "cell2"), T_FiT, T_FiT, T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "toDeleteCell1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "toDeleteCell2"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "toKeepCell"), T_EndT, T_BoolT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_RParenT, T_IsT, T_IfT, (T_Identifier "stateTree"), T_EqT, T_NullT, T_ThenT, T_ReturnT, T_TrueToken, T_ElseT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "stateTree"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursively"), T_LParenT, (T_Identifier "pointers"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteThisStateOnly"), T_LParenT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "deleteThisStateOnly"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "stateTree"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freeBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freePointers"), T_LParenT, (T_Identifier "pointers"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "front"), T_SepT, T_FreeT, (T_Identifier "stateTree"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "freePointers"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freePointersRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freePointersRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freePointersRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "front"), T_SepT, T_FreeT, (T_Identifier "pointers"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "freePointersRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_FreeT, (T_Identifier "front"), T_SepT, T_FreeT, (T_Identifier "rowPointers"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "deleteChildrenStateRecursively"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "deleteChildrenStateRecursivelyRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "rowPointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "cell1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "cell2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "deleteStateTreeRecursively"), T_LParenT, (T_Identifier "cell3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "askForAMove"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_CharT, (T_Identifier "playerSymbol"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_RParenT, T_IsT, T_IfT, (T_Identifier "currentTurn"), T_EqT, (T_Identifier "playerSymbol"), T_ThenT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "askForAMoveHuman"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "move"), T_RParenT, T_ElseT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "askForAMoveAI"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "playerSymbol"), T_CoT, (T_Identifier "aiData"), T_CoT, (T_Identifier "move"), T_RParenT, T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "placeMove"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "targetRow"), T_EqualT, T_NullT, T_SepT, T_IfT, (T_Identifier "moveRow"), T_LessEqT, (T_IntDigit "2"), T_ThenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_IfT, (T_Identifier "moveRow"), T_EqT, (T_IntDigit "1"), T_ThenT, (T_Identifier "targetRow"), T_EqualT, T_FstT, (T_Identifier "front"), T_ElseT, (T_Identifier "targetRow"), T_EqualT, T_SndT, (T_Identifier "front"), T_FiT, T_ElseT, (T_Identifier "targetRow"), T_EqualT, T_SndT, (T_Identifier "board"), T_FiT, T_SepT, T_IfT, (T_Identifier "moveColumn"), T_LessEqT, (T_IntDigit "2"), T_ThenT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "targetRow"), T_SepT, T_IfT, (T_Identifier "moveColumn"), T_EqT, (T_IntDigit "1"), T_ThenT, T_FstT, (T_Identifier "front"), T_EqualT, (T_Identifier "currentTurn"), T_ElseT, T_SndT, (T_Identifier "front"), T_EqualT, (T_Identifier "currentTurn"), T_FiT, T_ElseT, T_SndT, (T_Identifier "targetRow"), T_EqualT, (T_Identifier "currentTurn"), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "notifyMove"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "currentTurn"), T_CoT, T_CharT, (T_Identifier "playerSymbol"), T_CoT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_CoT, T_IntT, (T_Identifier "moveRow"), T_CoT, T_IntT, (T_Identifier "moveColumn"), T_RParenT, T_IsT, T_IfT, (T_Identifier "currentTurn"), T_EqT, (T_Identifier "playerSymbol"), T_ThenT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "notifyMoveAI"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "playerSymbol"), T_CoT, (T_Identifier "aiData"), T_CoT, (T_Identifier "moveRow"), T_CoT, (T_Identifier "moveColumn"), T_RParenT, T_ElseT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "notifyMoveHuman"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "playerSymbol"), T_CoT, (T_Identifier "moveRow"), T_CoT, (T_Identifier "moveColumn"), T_RParenT, T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_CharT, (T_Identifier "oppositeSymbol"), T_LParenT, T_CharT, (T_Identifier "symbol"), T_RParenT, T_IsT, T_IfT, (T_Identifier "symbol"), T_EqT, (T_CharLiteral "'x'"), T_ThenT, T_ReturnT, (T_CharLiteral "'o'"), T_ElseT, T_IfT, (T_Identifier "symbol"), T_EqT, (T_CharLiteral "'o'"), T_ThenT, T_ReturnT, (T_CharLiteral "'x'"), T_ElseT, T_PrintLnT, (T_StringLiteral "\"Internal Error: symbol given is neither \\'x\\' or \\'o\\'\""), T_SepT, T_ExitT, T_MinusToken, (T_IntDigit "1"), T_FiT, T_FiT, T_EndT, T_CharT, (T_Identifier "symbolAt"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_IntT, (T_Identifier "row"), T_CoT, T_IntT, (T_Identifier "column"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "targetRow"), T_EqualT, T_NullT, T_SepT, T_IfT, (T_Identifier "row"), T_LessEqT, (T_IntDigit "2"), T_ThenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_IfT, (T_Identifier "row"), T_EqT, (T_IntDigit "1"), T_ThenT, (T_Identifier "targetRow"), T_EqualT, T_FstT, (T_Identifier "front"), T_ElseT, (T_Identifier "targetRow"), T_EqualT, T_SndT, (T_Identifier "front"), T_FiT, T_ElseT, (T_Identifier "targetRow"), T_EqualT, T_SndT, (T_Identifier "board"), T_FiT, T_SepT, T_CharT, (T_Identifier "targetCell"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_IfT, (T_Identifier "column"), T_LessEqT, (T_IntDigit "2"), T_ThenT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "targetRow"), T_SepT, T_IfT, (T_Identifier "column"), T_EqT, (T_IntDigit "1"), T_ThenT, (T_Identifier "targetCell"), T_EqualT, T_FstT, (T_Identifier "front"), T_ElseT, (T_Identifier "targetCell"), T_EqualT, T_SndT, (T_Identifier "front"), T_FiT, T_ElseT, (T_Identifier "targetCell"), T_EqualT, T_SndT, (T_Identifier "targetRow"), T_FiT, T_SepT, T_ReturnT, (T_Identifier "targetCell"), T_EndT, T_BoolT, (T_Identifier "containEmptyCell"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "board"), T_SepT, T_BoolT, (T_Identifier "row1ContainEmpty"), T_EqualT, T_CallT, (T_Identifier "containEmptyCellRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "row2ContainEmpty"), T_EqualT, T_CallT, (T_Identifier "containEmptyCellRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "row3ContainEmpty"), T_EqualT, T_CallT, (T_Identifier "containEmptyCellRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "row1ContainEmpty"), T_OrT, (T_Identifier "row2ContainEmpty"), T_OrT, (T_Identifier "row3ContainEmpty"), T_EndT, T_BoolT, (T_Identifier "containEmptyCellRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "row"), T_SepT, T_CharT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_CharT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "row"), T_SepT, T_ReturnT, (T_Identifier "cell1"), T_EqT, (T_CharLiteral "'\\0'"), T_OrT, (T_Identifier "cell2"), T_EqT, (T_CharLiteral "'\\0'"), T_OrT, (T_Identifier "cell3"), T_EqT, (T_CharLiteral "'\\0'"), T_EndT, T_BoolT, (T_Identifier "hasWon"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_CoT, T_CharT, (T_Identifier "candidate"), T_RParenT, T_IsT, T_CharT, (T_Identifier "c11"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "1"), T_CoT, (T_IntDigit "1"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c12"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c13"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "1"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c21"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "1"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c22"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c23"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c31"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "3"), T_CoT, (T_IntDigit "1"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c32"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "3"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, T_CharT, (T_Identifier "c33"), T_EqualT, T_CallT, (T_Identifier "symbolAt"), T_LParenT, (T_Identifier "board"), T_CoT, (T_IntDigit "3"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "c11"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c12"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c13"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c21"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c22"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c23"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c31"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c32"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c33"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c11"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c21"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c31"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c12"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c22"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c32"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c13"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c23"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c33"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c11"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c22"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c33"), T_EqT, (T_Identifier "candidate"), T_OrT, (T_Identifier "c13"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c22"), T_EqT, (T_Identifier "candidate"), T_AndT, (T_Identifier "c31"), T_EqT, (T_Identifier "candidate"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "allocateNewBoard"), T_LParenT, T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_CallT, (T_Identifier "allocateNewRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_CallT, (T_Identifier "allocateNewRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_CallT, (T_Identifier "allocateNewRow"), T_LParenT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "row1"), T_CoT, (T_Identifier "row2"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "root"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "front"), T_CoT, (T_Identifier "row3"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "root"), T_EndT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "allocateNewRow"), T_LParenT, T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_NewpairT, T_LParenT, (T_CharLiteral "'\\0'"), T_CoT, (T_CharLiteral "'\\0'"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "root"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "front"), T_CoT, (T_CharLiteral "'\\0'"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "root"), T_EndT, T_BoolT, (T_Identifier "freeBoard"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "board"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "board"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freeRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freeRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freeRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "front"), T_SepT, T_FreeT, (T_Identifier "board"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "freeRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_CharT, T_RParenT, (T_Identifier "row"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "row"), T_SepT, T_FreeT, (T_Identifier "front"), T_SepT, T_FreeT, (T_Identifier "row"), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "printAiData"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_CharT, T_CoT, T_PairT, T_RParenT, (T_Identifier "info"), T_EqualT, T_FstT, (T_Identifier "aiData"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_EqualT, T_SndT, (T_Identifier "aiData"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printStateTreeRecursively"), T_LParenT, (T_Identifier "stateTree"), T_RParenT, T_SepT, T_ExitT, (T_IntDigit "0"), T_EndT, T_BoolT, (T_Identifier "printStateTreeRecursively"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "stateTree"), T_RParenT, T_IsT, T_IfT, (T_Identifier "stateTree"), T_EqT, T_NullT, T_ThenT, T_ReturnT, T_TrueToken, T_ElseT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "stateTree"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_IntT, (T_Identifier "value"), T_EqualT, T_SndT, (T_Identifier "stateTree"), T_SepT, T_PrintT, (T_CharLiteral "'v'"), T_SepT, T_PrintT, (T_CharLiteral "'='"), T_SepT, T_PrintLnT, (T_Identifier "value"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printChildrenStateTree"), T_LParenT, (T_Identifier "pointers"), T_RParenT, T_SepT, T_PrintLnT, (T_CharLiteral "'p'"), T_SepT, T_ReturnT, T_TrueToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "printChildrenStateTree"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "pointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "pointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "row3"), T_EqualT, T_SndT, (T_Identifier "pointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printChildrenStateTreeRow"), T_LParenT, (T_Identifier "row1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printChildrenStateTreeRow"), T_LParenT, (T_Identifier "row2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printChildrenStateTreeRow"), T_LParenT, (T_Identifier "row3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "printChildrenStateTreeRow"), T_LParenT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "rowPointers"), T_RParenT, T_IsT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "front"), T_EqualT, T_FstT, (T_Identifier "rowPointers"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell1"), T_EqualT, T_FstT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell2"), T_EqualT, T_SndT, (T_Identifier "front"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_IntT, T_RParenT, (T_Identifier "cell3"), T_EqualT, T_SndT, (T_Identifier "rowPointers"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printStateTreeRecursively"), T_LParenT, (T_Identifier "cell1"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printStateTreeRecursively"), T_LParenT, (T_Identifier "cell2"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printStateTreeRecursively"), T_LParenT, (T_Identifier "cell3"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_CharT, (T_Identifier "playerSymbol"), T_EqualT, T_CallT, (T_Identifier "chooseSymbol"), T_LParenT, T_RParenT, T_SepT, T_CharT, (T_Identifier "aiSymbol"), T_EqualT, T_CallT, (T_Identifier "oppositeSymbol"), T_LParenT, (T_Identifier "playerSymbol"), T_RParenT, T_SepT, T_CharT, (T_Identifier "currentTurn"), T_EqualT, (T_CharLiteral "'x'"), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "board"), T_EqualT, T_CallT, (T_Identifier "allocateNewBoard"), T_LParenT, T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"Initialising AI. Please wait, this may take a few minutes.\""), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "aiData"), T_EqualT, T_CallT, (T_Identifier "initAI"), T_LParenT, (T_Identifier "aiSymbol"), T_RParenT, T_SepT, T_IntT, (T_Identifier "turnCount"), T_EqualT, (T_IntDigit "0"), T_SepT, T_CharT, (T_Identifier "winner"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_BoolT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, T_WhileT, (T_Identifier "winner"), T_EqT, (T_CharLiteral "'\\0'"), T_AndT, (T_Identifier "turnCount"), T_LessT, (T_IntDigit "9"), T_DoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "move"), T_EqualT, T_LBracketT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_RBracketT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "askForAMove"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "playerSymbol"), T_CoT, (T_Identifier "aiData"), T_CoT, (T_Identifier "move"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "placeMove"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "notifyMove"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_CoT, (T_Identifier "playerSymbol"), T_CoT, (T_Identifier "aiData"), T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_CoT, (T_Identifier "move"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "printBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "won"), T_EqualT, T_CallT, (T_Identifier "hasWon"), T_LParenT, (T_Identifier "board"), T_CoT, (T_Identifier "currentTurn"), T_RParenT, T_SepT, T_IfT, (T_Identifier "won"), T_ThenT, (T_Identifier "winner"), T_EqualT, (T_Identifier "currentTurn"), T_ElseT, T_SkipT, T_FiT, T_SepT, (T_Identifier "currentTurn"), T_EqualT, T_CallT, (T_Identifier "oppositeSymbol"), T_LParenT, (T_Identifier "currentTurn"), T_RParenT, T_SepT, (T_Identifier "turnCount"), T_EqualT, (T_Identifier "turnCount"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "freeBoard"), T_LParenT, (T_Identifier "board"), T_RParenT, T_SepT, (T_Identifier "_"), T_EqualT, T_CallT, (T_Identifier "destroyAI"), T_LParenT, (T_Identifier "aiData"), T_RParenT, T_SepT, T_IfT, (T_Identifier "winner"), T_NotEqT, (T_CharLiteral "'\\0'"), T_ThenT, T_PrintT, (T_Identifier "winner"), T_SepT, T_PrintLnT, (T_StringLiteral "\" has won!\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"Stalemate!\""), T_FiT, T_EndT]

hashTable :: Assertion
hashTable = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/advanced/hashTable.wacc"))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "init"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "length"), T_EqualT, T_LenT, (T_Identifier "table"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "length"), T_DoT, (T_Identifier "table"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_EqualT, T_NullT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "contain"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_CoT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "index"), T_EqualT, T_CallT, (T_Identifier "calculateIndex"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "node"), T_EqualT, T_CallT, (T_Identifier "findNode"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "node"), T_NotEqT, T_NullT, T_EndT, T_BoolT, (T_Identifier "insertIfNotContain"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_CoT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "index"), T_EqualT, T_CallT, (T_Identifier "calculateIndex"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "node"), T_EqualT, T_CallT, (T_Identifier "findNode"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_IfT, (T_Identifier "node"), T_NotEqT, T_NullT, T_ThenT, T_ReturnT, T_FalseToken, T_ElseT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "x"), T_CoT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_EqualT, (T_Identifier "p"), T_SepT, T_ReturnT, T_TrueToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "remove"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_CoT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "index"), T_EqualT, T_CallT, (T_Identifier "calculateIndex"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "node"), T_EqualT, T_CallT, (T_Identifier "findNode"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_IfT, (T_Identifier "node"), T_EqT, T_NullT, T_ThenT, T_ReturnT, T_FalseToken, T_ElseT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_EqualT, T_CallT, (T_Identifier "removeNode"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "index"), T_RBracketT, T_CoT, (T_Identifier "node"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "removeAll"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "length"), T_EqualT, T_LenT, (T_Identifier "table"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "length"), T_DoT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, (T_Identifier "table"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_SepT, T_WhileT, (T_Identifier "p"), T_NotEqT, T_NullT, T_DoT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p2"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_FreeT, (T_Identifier "p"), T_SepT, (T_Identifier "p"), T_EqualT, (T_Identifier "p2"), T_DoneT, T_SepT, (T_Identifier "table"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_EqualT, T_NullT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_IntT, (T_Identifier "count"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "length"), T_EqualT, T_LenT, (T_Identifier "table"), T_SepT, T_IntT, (T_Identifier "sum"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "length"), T_DoT, T_IntT, (T_Identifier "subSum"), T_EqualT, T_CallT, (T_Identifier "countNodes"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "sum"), T_EqualT, (T_Identifier "sum"), T_PlusToken, (T_Identifier "subSum"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_ReturnT, (T_Identifier "sum"), T_EndT, T_BoolT, (T_Identifier "printAll"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "length"), T_EqualT, T_LenT, (T_Identifier "table"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "length"), T_DoT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "printAllNodes"), T_LParenT, (T_Identifier "table"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_RParenT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_IntT, (T_Identifier "calculateIndex"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_CoT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "length"), T_EqualT, T_LenT, (T_Identifier "table"), T_SepT, T_ReturnT, (T_Identifier "x"), T_ModuloT, (T_Identifier "length"), T_EndT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "findNode"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "head"), T_CoT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_WhileT, (T_Identifier "head"), T_NotEqT, T_NullT, T_DoT, T_IntT, (T_Identifier "y"), T_EqualT, T_FstT, (T_Identifier "head"), T_SepT, T_IfT, (T_Identifier "y"), T_EqT, (T_Identifier "x"), T_ThenT, T_ReturnT, (T_Identifier "head"), T_ElseT, (T_Identifier "head"), T_EqualT, T_SndT, (T_Identifier "head"), T_FiT, T_DoneT, T_SepT, T_ReturnT, T_NullT, T_EndT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "removeNode"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "head"), T_CoT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "toRemove"), T_RParenT, T_IsT, T_IfT, (T_Identifier "head"), T_EqT, T_NullT, T_ThenT, T_ReturnT, T_NullT, T_ElseT, T_IfT, (T_Identifier "head"), T_EqT, (T_Identifier "toRemove"), T_ThenT, (T_Identifier "head"), T_EqualT, T_SndT, (T_Identifier "head"), T_SepT, T_FreeT, (T_Identifier "toRemove"), T_SepT, T_ReturnT, (T_Identifier "head"), T_ElseT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "tail"), T_EqualT, T_SndT, (T_Identifier "head"), T_SepT, T_SndT, (T_Identifier "head"), T_EqualT, T_CallT, (T_Identifier "removeNode"), T_LParenT, (T_Identifier "tail"), T_CoT, (T_Identifier "toRemove"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "head"), T_FiT, T_FiT, T_EndT, T_IntT, (T_Identifier "countNodes"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "head"), T_RParenT, T_IsT, T_IntT, (T_Identifier "sum"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "head"), T_NotEqT, T_NullT, T_DoT, (T_Identifier "sum"), T_EqualT, (T_Identifier "sum"), T_PlusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "head"), T_EqualT, T_SndT, (T_Identifier "head"), T_DoneT, T_SepT, T_ReturnT, (T_Identifier "sum"), T_EndT, T_BoolT, (T_Identifier "printAllNodes"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "head"), T_RParenT, T_IsT, T_WhileT, (T_Identifier "head"), T_NotEqT, T_NullT, T_DoT, T_IntT, (T_Identifier "x"), T_EqualT, T_FstT, (T_Identifier "head"), T_SepT, T_PrintT, (T_Identifier "x"), T_SepT, T_PrintT, (T_CharLiteral "' '"), T_SepT, (T_Identifier "head"), T_EqualT, T_SndT, (T_Identifier "head"), T_DoneT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_CharT, (T_Identifier "printMenu"), T_LParenT, T_RParenT, T_IsT, T_PrintLnT, (T_StringLiteral "\"===========================================\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"========== Hash Table Program =============\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"===========================================\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=                                         =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= Please choose the following options:    =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=                                         =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= a: insert an integer                    =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= b: find an integer                      =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= c: count the integers                   =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= d: print all integers                   =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= e: remove an integer                    =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= f: remove all integers                  =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"= g: exit                                 =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"=                                         =\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"===========================================\""), T_SepT, T_IntT, (T_Identifier "minChoice"), T_EqualT, T_OrdT, (T_CharLiteral "'a'"), T_SepT, T_IntT, (T_Identifier "maxChoice"), T_EqualT, T_OrdT, (T_CharLiteral "'g'"), T_SepT, T_WhileT, T_TrueToken, T_DoT, T_PrintT, (T_StringLiteral "\"Your decision: \""), T_SepT, T_CharT, (T_Identifier "d"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_ReadT, (T_Identifier "d"), T_SepT, T_IntT, (T_Identifier "dInt"), T_EqualT, T_OrdT, (T_Identifier "d"), T_SepT, T_IfT, (T_Identifier "minChoice"), T_LessEqT, (T_Identifier "dInt"), T_AndT, (T_Identifier "dInt"), T_LessEqT, (T_Identifier "maxChoice"), T_ThenT, T_ReturnT, (T_Identifier "d"), T_ElseT, T_PrintT, (T_StringLiteral "\"You have entered: \""), T_SepT, T_PrintT, (T_Identifier "d"), T_SepT, T_PrintLnT, (T_StringLiteral "\" which is invalid, please try again.\""), T_FiT, T_DoneT, T_SepT, T_ReturnT, (T_CharLiteral "'\\0'"), T_EndT, T_IntT, (T_Identifier "askForInt"), T_LParenT, T_CharT, T_LBracketT, T_RBracketT, (T_Identifier "message"), T_RParenT, T_IsT, T_PrintT, (T_Identifier "message"), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintT, (T_StringLiteral "\"You have entered: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_ReturnT, (T_Identifier "x"), T_EndT, T_BoolT, (T_Identifier "handleMenuInsert"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "askForInt"), T_LParenT, (T_StringLiteral "\"Please enter an integer to insert: \""), T_RParenT, T_SepT, T_BoolT, (T_Identifier "notContain"), T_EqualT, T_CallT, (T_Identifier "insertIfNotContain"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_IfT, (T_Identifier "notContain"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"Successfully insert it. The integer is new.\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"The integer is already there. No insertion is made.\""), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "handleMenuFind"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "askForInt"), T_LParenT, (T_StringLiteral "\"Please enter an integer to find: \""), T_RParenT, T_SepT, T_BoolT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "contain"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"Find the integer.\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"The integer is not found.\""), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "handleMenuCount"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "size"), T_EqualT, T_CallT, (T_Identifier "count"), T_LParenT, (T_Identifier "table"), T_RParenT, T_SepT, T_IfT, (T_Identifier "size"), T_EqT, (T_IntDigit "1"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"There is only 1 integer.\""), T_ElseT, T_PrintT, (T_StringLiteral "\"There are \""), T_SepT, T_PrintT, (T_Identifier "size"), T_SepT, T_PrintLnT, (T_StringLiteral "\" integers.\""), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "handleMenuPrint"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_PrintT, (T_StringLiteral "\"Here are the integers: \""), T_SepT, T_BoolT, (T_Identifier "junk"), T_EqualT, T_CallT, (T_Identifier "printAll"), T_LParenT, (T_Identifier "table"), T_RParenT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "handleMenuRemove"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "askForInt"), T_LParenT, (T_StringLiteral "\"Please enter an integer to remove: \""), T_RParenT, T_SepT, T_BoolT, (T_Identifier "find"), T_EqualT, T_CallT, (T_Identifier "remove"), T_LParenT, (T_Identifier "table"), T_CoT, (T_Identifier "x"), T_RParenT, T_SepT, T_IfT, (T_Identifier "find"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"The integer has been removed.\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"The integer is not found.\""), T_FiT, T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "handleMenuRemoveAll"), T_LParenT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_RParenT, T_IsT, T_BoolT, (T_Identifier "junk"), T_EqualT, T_CallT, (T_Identifier "removeAll"), T_LParenT, (T_Identifier "table"), T_RParenT, T_SepT, T_PrintLnT, (T_StringLiteral "\"All integers have been removed.\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "table"), T_EqualT, T_LBracketT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_CoT, T_NullT, T_RBracketT, T_SepT, T_BoolT, (T_Identifier "junk"), T_EqualT, T_CallT, (T_Identifier "init"), T_LParenT, (T_Identifier "table"), T_RParenT, T_SepT, T_BoolT, (T_Identifier "continue"), T_EqualT, T_TrueToken, T_SepT, T_WhileT, (T_Identifier "continue"), T_DoT, T_CharT, (T_Identifier "choice"), T_EqualT, T_CallT, (T_Identifier "printMenu"), T_LParenT, T_RParenT, T_SepT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'a'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuInsert"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'b'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuFind"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'c'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuCount"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'d'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuPrint"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'e'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuRemove"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'f'"), T_ThenT, T_BoolT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "handleMenuRemoveAll"), T_LParenT, (T_Identifier "table"), T_RParenT, T_ElseT, T_IfT, (T_Identifier "choice"), T_EqT, (T_CharLiteral "'g'"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"Goodbye Human\""), T_SepT, (T_Identifier "continue"), T_EqualT, T_FalseToken, T_ElseT, T_PrintT, (T_StringLiteral "\"Error: unknown choice (\""), T_SepT, T_PrintT, (T_Identifier "choice"), T_SepT, T_PrintLnT, (T_StringLiteral "\")\""), T_SepT, T_ExitT, T_MinusToken, (T_IntDigit "1"), T_FiT, T_FiT, T_FiT, T_FiT, T_FiT, T_FiT, T_FiT, T_DoneT, T_EndT]

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
array = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/array.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_CoT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_IntDigit "10"), T_DoT, (T_Identifier "a"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_EqualT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_StringLiteral "\" = {\""), T_SepT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_IntDigit "10"), T_DoT, T_PrintT, (T_Identifier "a"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_SepT, T_IfT, (T_Identifier "i"), T_LessT, (T_IntDigit "9"), T_ThenT, T_PrintT, (T_StringLiteral "\", \""), T_ElseT, T_SkipT, T_FiT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"}\""), T_EndT]

arrayBasic :: Assertion
arrayBasic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayBasic.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EndT]

arrayEmpty :: Assertion
arrayEmpty = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayEmpty.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, T_RBracketT, T_EndT]

arrayLength :: Assertion
arrayLength = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayLength.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "43"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "18"), T_CoT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintLnT, T_LenT, (T_Identifier "a"), T_EndT]

arrayLookup :: Assertion
arrayLookup = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayLookup.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "43"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "18"), T_CoT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EndT]

arrayNested :: Assertion
arrayNested = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayNested.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "b"), T_EqualT, T_LBracketT, (T_IntDigit "3"), T_CoT, (T_IntDigit "4"), T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, T_LBracketT, T_RBracketT, (T_Identifier "c"), T_EqualT, T_LBracketT, (T_Identifier "a"), T_CoT, (T_Identifier "b"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "c"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "c"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EndT]

arrayPrint :: Assertion
arrayPrint = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arrayPrint.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "0"), T_CoT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_CoT, (T_IntDigit "4"), T_CoT, (T_IntDigit "5"), T_CoT, (T_IntDigit "6"), T_CoT, (T_IntDigit "7"), T_CoT, (T_IntDigit "8"), T_CoT, (T_IntDigit "9"), T_RBracketT, T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_StringLiteral "\" = {\""), T_SepT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_IntDigit "10"), T_DoT, T_PrintT, (T_Identifier "a"), T_LBracketT, (T_Identifier "i"), T_RBracketT, T_SepT, T_IfT, (T_Identifier "i"), T_LessT, (T_IntDigit "9"), T_ThenT, T_PrintT, (T_StringLiteral "\", \""), T_ElseT, T_SkipT, T_FiT, T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"}\""), T_EndT]

arraySimple :: Assertion
arraySimple = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/arraySimple.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_IntDigit "42"), T_SepT, T_PrintLnT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EndT]

modifyString :: Assertion
modifyString = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/modifyString.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"hello world!\""), T_SepT, T_PrintLnT, (T_Identifier "s"), T_SepT, (T_Identifier "s"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_CharLiteral "'H'"), T_SepT, T_PrintLnT, (T_Identifier "s"), T_SepT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"Hi!\""), T_SepT, T_PrintLnT, (T_Identifier "s"), T_EndT]

printRef :: Assertion
printRef = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/array/printRef.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"Printing an array variable gives an address, such as \""), T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "a"), T_EndT]

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
exit1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/exit/exit-1.wacc" ))
  @=? [ T_BeginT, T_ExitT, T_MinusToken, (T_IntDigit "1"), T_EndT]

exitBasic :: Assertion
exitBasic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/exit/exitBasic.wacc" ))
  @=? [ T_BeginT, T_ExitT, (T_IntDigit "7"), T_EndT]

exitBasic2 :: Assertion
exitBasic2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/exit/exitBasic2.wacc" ))
  @=? [ T_BeginT, T_ExitT, (T_IntDigit "42"), T_EndT]

exitWrap :: Assertion
exitWrap = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/exit/exitWrap.wacc" ))
  @=? [ T_BeginT, T_ExitT, (T_IntDigit "256"), T_EndT]

skipTests :: [TestTree]
skipTests =
  [ testCase "Comment" comment
  , testCase "Comment In Line" commentInLine
  , testCase "Skip" skip
  ]

comment :: Assertion
comment = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/skip/comment.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_EndT]

commentInLine :: Assertion
commentInLine = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/skip/commentInLine.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_EndT]

skip :: Assertion
skip = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/basic/skip/skip.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_EndT]

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
andExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/andExpr.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "a"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, T_FalseToken, T_SepT, T_PrintLnT, (T_Identifier "a"), T_AndT, (T_Identifier "b"), T_SepT, T_PrintLnT, (T_Identifier "a"), T_AndT, T_TrueToken, T_SepT, T_PrintLnT, (T_Identifier "b"), T_AndT, T_FalseToken, T_EndT]

boolCalc :: Assertion
boolCalc = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/boolCalc.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b1"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "b2"), T_EqualT, T_FalseToken, T_SepT, T_BoolT, (T_Identifier "b3"), T_EqualT, (T_Identifier "b1"), T_AndT, (T_Identifier "b2"), T_SepT, T_PrintLnT, (T_Identifier "b3"), T_EndT]

boolExpr1 :: Assertion
boolExpr1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/boolExpr1.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_NotT, T_LParenT, T_LParenT, T_TrueToken, T_AndT, T_FalseToken, T_RParenT, T_OrT, T_LParenT, T_TrueToken, T_AndT, T_FalseToken, T_RParenT, T_RParenT, T_SepT, T_IfT, (T_Identifier "b"), T_EqT, T_TrueToken, T_ThenT, T_PrintLnT, (T_StringLiteral "\"Correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"Wrong\""), T_FiT, T_EndT]
charComparisonExpr :: Assertion
charComparisonExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/charComparisonExpr.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c1"), T_EqualT, (T_CharLiteral "'a'"), T_SepT, T_CharT, (T_Identifier "c2"), T_EqualT, (T_CharLiteral "'z'"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_EqT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_NotEqT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_LessT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_LessEqT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_GreaterT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_GreaterEqT, (T_Identifier "c2"), T_EndT]

divExpr :: Assertion
divExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/divExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_DivideT, (T_Identifier "y"), T_EndT]

equalsExpr :: Assertion
equalsExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/equalsExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, (T_Identifier "x"), T_EqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_EqT, (T_Identifier "z"), T_EndT]

greaterEqExpr :: Assertion
greaterEqExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/greaterEqExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "4"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_GreaterEqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_GreaterEqT, (T_Identifier "z"), T_SepT, T_PrintLnT, (T_Identifier "z"), T_GreaterEqT, (T_Identifier "z"), T_EndT]

greaterExpr :: Assertion
greaterExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/greaterExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_GreaterT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_GreaterT, (T_Identifier "z"), T_EndT]

intCalc :: Assertion
intCalc = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/intCalc.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "42"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "30"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "z"), T_EndT]

intExpr1 :: Assertion
intExpr1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/intExpr1.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, T_LParenT, (T_IntDigit "10"), T_TimesT, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "2"), T_TimesT, (T_IntDigit "15"), T_RParenT, T_SepT, T_IfT, (T_Identifier "a"), T_EqT, (T_IntDigit "40"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"Correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"Wrong\""), T_FiT, T_EndT]

lessCharExpr :: Assertion
lessCharExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/lessCharExpr.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "a"), T_EqualT, (T_CharLiteral "'a'"), T_SepT, T_CharT, (T_Identifier "e"), T_EqualT, (T_CharLiteral "'e'"), T_SepT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'c'"), T_SepT, T_PrintLnT, (T_Identifier "a"), T_LessT, (T_Identifier "e"), T_SepT, T_PrintLnT, (T_Identifier "e"), T_LessT, (T_Identifier "c"), T_EndT]

lessEqExpr :: Assertion
lessEqExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/lessEqExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "4"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_LessEqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_LessEqT, (T_Identifier "z"), T_SepT, T_PrintLnT, (T_Identifier "z"), T_LessEqT, (T_Identifier "a"), T_EndT]

lessExpr :: Assertion
lessExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/lessExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_LessT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_LessT, (T_Identifier "z"), T_EndT]

longExpr :: Assertion
longExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/longExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_PlusToken, T_LParenT, (T_IntDigit "2"), T_PlusToken, T_LParenT, (T_IntDigit "3"), T_PlusToken, T_LParenT, (T_IntDigit "4"), T_PlusToken, T_LParenT, (T_IntDigit "5"), T_PlusToken, T_LParenT, (T_IntDigit "6"), T_PlusToken, T_LParenT, (T_IntDigit "7"), T_PlusToken, T_LParenT, (T_IntDigit "8"), T_PlusToken, T_LParenT, (T_IntDigit "9"), T_PlusToken, T_LParenT, (T_IntDigit "10"), T_PlusToken, T_LParenT, (T_IntDigit "11"), T_PlusToken, T_LParenT, (T_IntDigit "12"), T_PlusToken, T_LParenT, (T_IntDigit "13"), T_PlusToken, T_LParenT, (T_IntDigit "14"), T_PlusToken, T_LParenT, (T_IntDigit "15"), T_PlusToken, T_LParenT, (T_IntDigit "16"), T_PlusToken, (T_IntDigit "17"), T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_RParenT, T_SepT, T_ExitT, (T_Identifier "x"), T_EndT]

longExpr2 :: Assertion
longExpr2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/longExpr2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_LParenT, (T_IntDigit "2"), T_PlusToken, (T_IntDigit "3"), T_PlusToken, (T_IntDigit "2"), T_PlusToken, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "1"), T_RParenT, T_MinusToken, T_LParenT, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "2"), T_RParenT, T_TimesT, T_LParenT, (T_IntDigit "3"), T_MinusToken, (T_IntDigit "4"), T_DivideT, (T_IntDigit "6"), T_RParenT, T_DivideT, T_LParenT, (T_IntDigit "2"), T_TimesT, T_LParenT, (T_IntDigit "18"), T_MinusToken, (T_IntDigit "17"), T_RParenT, T_PlusToken, T_LParenT, (T_IntDigit "3"), T_TimesT, (T_IntDigit "4"), T_DivideT, (T_IntDigit "4"), T_PlusToken, (T_IntDigit "6"), T_RParenT, T_RParenT, T_SepT, T_ExitT, (T_Identifier "x"), T_EndT]

longExpr3 :: Assertion
longExpr3 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/longExpr3.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, T_LParenT, (T_IntDigit "1"), T_MinusToken, (T_IntDigit "2"), T_RParenT, T_PlusToken, (T_IntDigit "3"), T_RParenT, T_MinusToken, (T_IntDigit "4"), T_RParenT, T_PlusToken, (T_IntDigit "5"), T_RParenT, T_MinusToken, (T_IntDigit "6"), T_RParenT, T_PlusToken, (T_IntDigit "7"), T_RParenT, T_MinusToken, (T_IntDigit "8"), T_RParenT, T_PlusToken, (T_IntDigit "9"), T_RParenT, T_MinusToken, (T_IntDigit "10"), T_RParenT, T_PlusToken, (T_IntDigit "11"), T_RParenT, T_MinusToken, (T_IntDigit "12"), T_RParenT, T_PlusToken, (T_IntDigit "13"), T_RParenT, T_MinusToken, (T_IntDigit "14"), T_RParenT, T_PlusToken, (T_IntDigit "15"), T_RParenT, T_MinusToken, (T_IntDigit "16"), T_RParenT, T_PlusToken, (T_IntDigit "17"), T_RParenT, T_SepT, T_ExitT, (T_Identifier "x"), T_EndT]

longSplitExpr :: Assertion
longSplitExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/longSplitExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "b"), T_EqualT, (T_IntDigit "3"), T_PlusToken, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "c"), T_EqualT, (T_IntDigit "5"), T_PlusToken, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "d"), T_EqualT, (T_IntDigit "7"), T_PlusToken, (T_IntDigit "8"), T_SepT, T_IntT, (T_Identifier "e"), T_EqualT, (T_IntDigit "9"), T_PlusToken, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "f"), T_EqualT, (T_IntDigit "11"), T_PlusToken, (T_IntDigit "12"), T_SepT, T_IntT, (T_Identifier "g"), T_EqualT, (T_IntDigit "13"), T_PlusToken, (T_IntDigit "14"), T_SepT, T_IntT, (T_Identifier "h"), T_EqualT, (T_IntDigit "15"), T_PlusToken, (T_IntDigit "16"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "17"), T_SepT, T_ExitT, (T_Identifier "a"), T_PlusToken, (T_Identifier "b"), T_PlusToken, (T_Identifier "c"), T_PlusToken, (T_Identifier "d"), T_PlusToken, (T_Identifier "e"), T_PlusToken, (T_Identifier "f"), T_PlusToken, (T_Identifier "g"), T_PlusToken, (T_Identifier "h"), T_PlusToken, (T_Identifier "i"), T_EndT]

longSplitExpr2 :: Assertion
longSplitExpr2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/longSplitExpr2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "2"), T_PlusToken, (T_IntDigit "3"), T_PlusToken, (T_IntDigit "4"), T_PlusToken, (T_IntDigit "5"), T_PlusToken, (T_IntDigit "6"), T_PlusToken, (T_IntDigit "7"), T_PlusToken, (T_IntDigit "8"), T_PlusToken, (T_IntDigit "9"), T_PlusToken, (T_IntDigit "10"), T_PlusToken, (T_IntDigit "11"), T_PlusToken, (T_IntDigit "12"), T_PlusToken, (T_IntDigit "13"), T_PlusToken, (T_IntDigit "14"), T_PlusToken, (T_IntDigit "15"), T_PlusToken, (T_IntDigit "16"), T_PlusToken, (T_IntDigit "17"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_MinusToken, (T_IntDigit "1"), T_MinusToken, (T_IntDigit "2"), T_MinusToken, (T_IntDigit "3"), T_MinusToken, (T_IntDigit "4"), T_MinusToken, (T_IntDigit "5"), T_MinusToken, (T_IntDigit "6"), T_MinusToken, (T_IntDigit "7"), T_MinusToken, (T_IntDigit "8"), T_MinusToken, (T_IntDigit "9"), T_MinusToken, (T_IntDigit "10"), T_MinusToken, (T_IntDigit "11"), T_MinusToken, (T_IntDigit "12"), T_MinusToken, (T_IntDigit "13"), T_MinusToken, (T_IntDigit "14"), T_MinusToken, (T_IntDigit "15"), T_MinusToken, (T_IntDigit "16"), T_MinusToken, (T_IntDigit "17"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "1"), T_TimesT, (T_IntDigit "2"), T_TimesT, (T_IntDigit "3"), T_TimesT, (T_IntDigit "4"), T_TimesT, (T_IntDigit "5"), T_TimesT, (T_IntDigit "6"), T_TimesT, (T_IntDigit "7"), T_TimesT, (T_IntDigit "8"), T_TimesT, (T_IntDigit "9"), T_TimesT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "div"), T_EqualT, (T_IntDigit "10"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_PlusToken, (T_Identifier "y"), T_PlusToken, T_LParenT, (T_Identifier "z"), T_DivideT, (T_Identifier "div"), T_RParenT, T_SepT, T_PrintLnT, T_LParenT, (T_Identifier "x"), T_PlusToken, (T_Identifier "y"), T_PlusToken, T_LParenT, (T_Identifier "z"), T_DivideT, (T_Identifier "div"), T_RParenT, T_RParenT, T_ModuloT, (T_IntDigit "256"), T_SepT, T_ExitT, (T_Identifier "x"), T_PlusToken, (T_Identifier "y"), T_PlusToken, T_LParenT, (T_Identifier "z"), T_DivideT, (T_Identifier "div"), T_RParenT, T_EndT]

minusExpr :: Assertion
minusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/minusExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "15"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "20"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_MinusToken, (T_Identifier "x"), T_EndT]

minusMinusExpr :: Assertion
minusMinusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/minusMinusExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_MinusToken, T_MinusToken, (T_IntDigit "2"), T_EndT]

minusNoWhitespaceExpr :: Assertion
minusNoWhitespaceExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/minusNoWhitespaceExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_MinusToken, (T_IntDigit "2"), T_EndT] 

minusPlusExpr :: Assertion
minusPlusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/minusPlusExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_MinusToken, T_PlusToken, (T_IntDigit "2"), T_EndT] 

modExpr :: Assertion
modExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/modExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_ModuloT, (T_Identifier "y"), T_EndT]

multExpr :: Assertion
multExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/multExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_TimesT, (T_Identifier "y"), T_EndT]

multNoWhitespaceExpr :: Assertion
multNoWhitespaceExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/multNoWhitespaceExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_TimesT, (T_IntDigit "2"), T_EndT]

negBothDiv :: Assertion
negBothDiv = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negBothDiv.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_MinusToken, (T_IntDigit "2"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_DivideT, (T_Identifier "y"), T_EndT]

negBothMod :: Assertion
negBothMod = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negBothMod.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_MinusToken, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_ModuloT, (T_Identifier "y"), T_EndT]

negDividendDiv :: Assertion
negDividendDiv = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negDividendDiv.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "2"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_DivideT, (T_Identifier "y"), T_EndT]

negDividendMod :: Assertion
negDividendMod = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negDividendMod.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_ModuloT, (T_Identifier "y"), T_EndT]

negDivisorDiv :: Assertion
negDivisorDiv = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negDivisorDiv.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_MinusToken, (T_IntDigit "2"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_DivideT, (T_Identifier "y"), T_EndT]

negDivisorMod :: Assertion
negDivisorMod = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negDivisorMod.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_MinusToken, (T_IntDigit "3"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_ModuloT, (T_Identifier "y"), T_EndT]

negExpr :: Assertion
negExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/negExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "42"), T_SepT, T_PrintLnT, T_MinusToken, (T_Identifier "x"), T_EndT]

notExpr :: Assertion
notExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/notExpr.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "a"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, T_FalseToken, T_SepT, T_PrintLnT, T_NotT, (T_Identifier "a"), T_SepT, T_PrintLnT, T_NotT, (T_Identifier "b"), T_EndT]

notequalsExpr :: Assertion
notequalsExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/notequalsExpr.wacc" ))
  @=?  [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, (T_IntDigit "4"), T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, (T_Identifier "x"), T_NotEqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_NotEqT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_NotEqT, (T_Identifier "z"), T_EndT]

orExpr :: Assertion
orExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/orExpr.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "a"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, T_FalseToken, T_SepT, T_PrintLnT, (T_Identifier "a"), T_OrT, (T_Identifier "b"), T_SepT, T_PrintLnT, (T_Identifier "a"), T_OrT, T_TrueToken, T_SepT, T_PrintLnT, (T_Identifier "b"), T_OrT, T_FalseToken, T_EndT]

ordAndchrExpr :: Assertion
ordAndchrExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/ordAndchrExpr.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "a"), T_EqualT, (T_CharLiteral "'a'"), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "99"), T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_StringLiteral "\" is \""), T_SepT, T_PrintLnT, T_OrdT, (T_Identifier "a"), T_SepT, T_PrintT, (T_Identifier "i"), T_SepT, T_PrintT, (T_StringLiteral "\" is \""), T_SepT, T_PrintLnT, T_ChrT, (T_Identifier "i"), T_EndT]

plusExpr :: Assertion
plusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/plusExpr.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "15"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "20"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_PlusToken, (T_Identifier "y"), T_EndT]

plusMinusExpr :: Assertion
plusMinusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/plusMinusExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_PlusToken, T_MinusToken, (T_IntDigit "2"), T_EndT]

plusNoWhitespaceExpr :: Assertion
plusNoWhitespaceExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/plusNoWhitespaceExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_PlusToken, (T_IntDigit "2"), T_EndT]

plusPlusExpr :: Assertion
plusPlusExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/plusPlusExpr.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_IntDigit "1"), T_PlusToken, T_PlusToken, (T_IntDigit "2"), T_EndT]

sequentialCount :: Assertion
sequentialCount = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/sequentialCount.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Can you count to 10?\""), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_EndT]

stringEqualsExpr :: Assertion
stringEqualsExpr = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/expressions/stringEqualsExpr.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s1"), T_EqualT, (T_StringLiteral "\"Hello\""), T_SepT, T_StringT, (T_Identifier "s2"), T_EqualT, (T_StringLiteral "\"foo\""), T_SepT, T_StringT, (T_Identifier "s3"), T_EqualT, (T_StringLiteral "\"foo\""), T_SepT, T_BoolT, (T_Identifier "b"), T_EqualT, (T_Identifier "s1"), T_EqT, (T_Identifier "s1"), T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, T_PrintLnT, (T_Identifier "s1"), T_EqT, (T_Identifier "s2"), T_SepT, T_PrintLnT, (T_Identifier "s2"), T_EqT, (T_Identifier "s3"), T_EndT]

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
asciiTable = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/asciiTable.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "printLine"), T_LParenT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_Identifier "n"), T_DoT, T_PrintT, (T_StringLiteral "\"-\""), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_BoolT, (T_Identifier "printMap"), T_LParenT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_PrintT, (T_StringLiteral "\"|  \""), T_SepT, T_IfT, (T_Identifier "n"), T_LessT, (T_IntDigit "100"), T_ThenT, T_PrintT, (T_StringLiteral "\" \""), T_ElseT, T_SkipT, T_FiT, T_SepT, T_PrintT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\" = \""), T_SepT, T_PrintT, T_ChrT, (T_Identifier "n"), T_SepT, T_PrintLnT, (T_StringLiteral "\"  |\""), T_SepT, T_ReturnT, T_TrueToken, T_EndT, T_PrintLnT, (T_StringLiteral "\"Asci character lookup table:\""), T_SepT, T_BoolT, (T_Identifier "r"), T_EqualT, T_CallT, (T_Identifier "printLine"), T_LParenT, (T_IntDigit "13"), T_RParenT, T_SepT, T_IntT, (T_Identifier "num"), T_EqualT, T_OrdT, (T_CharLiteral "' '"), T_SepT, T_WhileT, (T_Identifier "num"), T_LessT, (T_IntDigit "127"), T_DoT, (T_Identifier "r"), T_EqualT, T_CallT, (T_Identifier "printMap"), T_LParenT, (T_Identifier "num"), T_RParenT, T_SepT, (T_Identifier "num"), T_EqualT, (T_Identifier "num"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, (T_Identifier "r"), T_EqualT, T_CallT, (T_Identifier "printLine"), T_LParenT, (T_IntDigit "13"), T_RParenT, T_EndT]

functionDeclaration :: Assertion
functionDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/functionDeclaration.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "f"), T_LParenT, T_RParenT, T_IsT, T_ReturnT, (T_IntDigit "0"), T_EndT, T_SkipT, T_EndT]

functionManyArguments :: Assertion
functionManyArguments = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/functionManyArguments.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "doSomething"), T_LParenT, T_IntT, (T_Identifier "a"), T_CoT, T_BoolT, (T_Identifier "b"), T_CoT, T_CharT, (T_Identifier "c"), T_CoT, T_StringT, (T_Identifier "d"), T_CoT, T_BoolT, T_LBracketT, T_RBracketT, (T_Identifier "e"), T_CoT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "f"), T_RParenT, T_IsT, T_PrintT, (T_StringLiteral "\"a is \""), T_SepT, T_PrintLnT, (T_Identifier "a"), T_SepT, T_PrintT, (T_StringLiteral "\"b is \""), T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, T_PrintT, (T_StringLiteral "\"c is \""), T_SepT, T_PrintLnT, (T_Identifier "c"), T_SepT, T_PrintT, (T_StringLiteral "\"d is \""), T_SepT, T_PrintLnT, (T_Identifier "d"), T_SepT, T_PrintT, (T_StringLiteral "\"e is \""), T_SepT, T_PrintLnT, (T_Identifier "e"), T_SepT, T_PrintT, (T_StringLiteral "\"f is \""), T_SepT, T_PrintLnT, (T_Identifier "f"), T_SepT, T_ReturnT, (T_CharLiteral "'g'"), T_EndT, T_BoolT, T_LBracketT, T_RBracketT, (T_Identifier "bools"), T_EqualT, T_LBracketT, T_FalseToken, T_CoT, T_TrueToken, T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "ints"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_CharT, (T_Identifier "answer"), T_EqualT, T_CallT, (T_Identifier "doSomething"), T_LParenT, (T_IntDigit "42"), T_CoT, T_TrueToken, T_CoT, (T_CharLiteral "'u'"), T_CoT, (T_StringLiteral "\"hello\""), T_CoT, (T_Identifier "bools"), T_CoT, (T_Identifier "ints"), T_RParenT, T_SepT, T_PrintT, (T_StringLiteral "\"answer is \""), T_SepT, T_PrintLnT, (T_Identifier "answer"), T_EndT]

functionReturnPair :: Assertion
functionReturnPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/functionReturnPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "getPair"), T_LParenT, T_RParenT, T_IsT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_IntDigit "15"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "p"), T_EndT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_CallT, (T_Identifier "getPair"), T_LParenT, T_RParenT, T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

functionSimple :: Assertion
functionSimple = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/functionSimple.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "f"), T_LParenT, T_RParenT, T_IsT, T_ReturnT, (T_IntDigit "0"), T_EndT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

functionUpdateParameter :: Assertion
functionUpdateParameter = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/functionUpdateParameter.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "f"), T_LParenT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_PrintT, (T_StringLiteral "\"x is \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_PrintT, (T_StringLiteral "\"x is now \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_ReturnT, (T_Identifier "x"), T_EndT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintT, (T_StringLiteral "\"y is \""), T_SepT, T_PrintLnT, (T_Identifier "y"), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, (T_Identifier "y"), T_RParenT, T_SepT, T_PrintT, (T_StringLiteral "\"y is still \""), T_SepT, T_PrintLnT, (T_Identifier "y"), T_EndT]

incFunction :: Assertion
incFunction = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/incFunction.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "inc"), T_LParenT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "1"), T_EndT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "inc"), T_LParenT, (T_Identifier "x"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "inc"), T_LParenT, (T_Identifier "x"), T_RParenT, T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "inc"), T_LParenT, (T_Identifier "x"), T_RParenT, T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "inc"), T_LParenT, (T_Identifier "x"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

negFunction :: Assertion
negFunction = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/negFunction.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "neg"), T_LParenT, T_BoolT, (T_Identifier "b"), T_RParenT, T_IsT, T_ReturnT, T_NotT, (T_Identifier "b"), T_EndT, T_BoolT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, (T_Identifier "b"), T_EqualT, T_CallT, (T_Identifier "neg"), T_LParenT, (T_Identifier "b"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "b"), T_SepT, (T_Identifier "b"), T_EqualT, T_CallT, (T_Identifier "neg"), T_LParenT, (T_Identifier "b"), T_RParenT, T_SepT, (T_Identifier "b"), T_EqualT, T_CallT, (T_Identifier "neg"), T_LParenT, (T_Identifier "b"), T_RParenT, T_SepT, (T_Identifier "b"), T_EqualT, T_CallT, (T_Identifier "neg"), T_LParenT, (T_Identifier "b"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "b"), T_EndT]

sameArgName :: Assertion
sameArgName = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/sameArgName.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "f"), T_LParenT, T_IntT, (T_Identifier "f"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "f"), T_EndT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, (T_IntDigit "99"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

sameArgName2 :: Assertion
sameArgName2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/simple_functions/sameArgName2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "f"), T_LParenT, T_IntT, (T_Identifier "f"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "f"), T_EndT, T_IntT, (T_Identifier "f"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, (T_IntDigit "99"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "f"), T_EndT]

nestedFunctionsTests :: [TestTree]
nestedFunctionsTests =
  [ testCase "Fibonacci Full Rec" fibonacciFullRec
  , testCase "Fibonacci Recursive" fibonacciRecursive
  , testCase "Fixed Point Real Arithmetic" fixedPointRealArithmetic
  , testCase "Function Conditional Return" functionConditionalReturn
  ]

fibonacciFullRec :: Assertion
fibonacciFullRec = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/nested_functions/fibonacciFullRec.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "fibonacci"), T_LParenT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IfT, (T_Identifier "n"), T_LessEqT, (T_IntDigit "1"), T_ThenT, T_ReturnT, (T_Identifier "n"), T_ElseT, T_SkipT, T_FiT, T_SepT, T_IntT, (T_Identifier "f1"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_Identifier "n"), T_MinusToken, (T_IntDigit "1"), T_RParenT, T_SepT, T_IntT, (T_Identifier "f2"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_Identifier "n"), T_MinusToken, (T_IntDigit "2"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "f1"), T_PlusToken, (T_Identifier "f2"), T_EndT, T_PrintLnT, (T_StringLiteral "\"This program calculates the nth fibonacci number recursively.\""), T_SepT, T_PrintT, (T_StringLiteral "\"Please enter n (should not be too large): \""), T_SepT, T_IntT, (T_Identifier "n"), T_EqualT, (T_IntDigit "0"), T_SepT, T_ReadT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\"The input n is \""), T_SepT, T_PrintLnT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\"The nth fibonacci number is \""), T_SepT, T_IntT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_Identifier "n"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "result"), T_EndT]

fibonacciRecursive :: Assertion
fibonacciRecursive = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/nested_functions/fibonacciRecursive.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "fibonacci"), T_LParenT, T_IntT, (T_Identifier "n"), T_CoT, T_BoolT, (T_Identifier "toPrint"), T_RParenT, T_IsT, T_IfT, (T_Identifier "n"), T_LessEqT, (T_IntDigit "1"), T_ThenT, T_ReturnT, (T_Identifier "n"), T_ElseT, T_SkipT, T_FiT, T_SepT, T_IntT, (T_Identifier "f1"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_Identifier "n"), T_MinusToken, (T_IntDigit "1"), T_CoT, (T_Identifier "toPrint"), T_RParenT, T_SepT, T_IfT, (T_Identifier "toPrint"), T_ThenT, T_PrintT, (T_Identifier "f1"), T_SepT, T_PrintT, (T_StringLiteral "\", \""), T_ElseT, T_SkipT, T_FiT, T_SepT, T_IntT, (T_Identifier "f2"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_Identifier "n"), T_MinusToken, (T_IntDigit "2"), T_CoT, T_FalseToken, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "f1"), T_PlusToken, (T_Identifier "f2"), T_EndT, T_PrintLnT, (T_StringLiteral "\"The first 20 fibonacci numbers are:\""), T_SepT, T_PrintT, (T_StringLiteral "\"0, \""), T_SepT, T_IntT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "fibonacci"), T_LParenT, (T_IntDigit "19"), T_CoT, T_TrueToken, T_RParenT, T_SepT, T_PrintT, (T_Identifier "result"), T_SepT, T_PrintLnT, (T_StringLiteral "\"...\""), T_EndT]

fixedPointRealArithmetic :: Assertion
fixedPointRealArithmetic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/nested_functions/fixedPointRealArithmetic.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "q"), T_LParenT, T_RParenT, T_IsT, T_ReturnT, (T_IntDigit "14"), T_EndT, T_IntT, (T_Identifier "power"), T_LParenT, T_IntT, (T_Identifier "base"), T_CoT, T_IntT, (T_Identifier "amount"), T_RParenT, T_IsT, T_IntT, (T_Identifier "result"), T_EqualT, (T_IntDigit "1"), T_SepT, T_WhileT, (T_Identifier "amount"), T_GreaterT, (T_IntDigit "0"), T_DoT, (T_Identifier "result"), T_EqualT, (T_Identifier "result"), T_TimesT, (T_Identifier "base"), T_SepT, (T_Identifier "amount"), T_EqualT, (T_Identifier "amount"), T_MinusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_ReturnT, (T_Identifier "result"), T_EndT, T_IntT, (T_Identifier "f"), T_LParenT, T_RParenT, T_IsT, T_IntT, (T_Identifier "qq"), T_EqualT, T_CallT, (T_Identifier "q"), T_LParenT, T_RParenT, T_SepT, T_IntT, (T_Identifier "f"), T_EqualT, T_CallT, (T_Identifier "power"), T_LParenT, (T_IntDigit "2"), T_CoT, (T_Identifier "qq"), T_RParenT, T_SepT, T_ReturnT, (T_Identifier "f"), T_EndT, T_IntT, (T_Identifier "intToFixedPoint"), T_LParenT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "n"), T_TimesT, (T_Identifier "ff"), T_EndT, T_IntT, (T_Identifier "fixedPointToIntRoundDown"), T_LParenT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "x"), T_DivideT, (T_Identifier "ff"), T_EndT, T_IntT, (T_Identifier "fixedPointToIntRoundNear"), T_LParenT, T_IntT, (T_Identifier "x"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_IfT, (T_Identifier "x"), T_GreaterEqT, (T_IntDigit "0"), T_ThenT, T_ReturnT, T_LParenT, (T_Identifier "x"), T_PlusToken, (T_Identifier "ff"), T_DivideT, (T_IntDigit "2"), T_RParenT, T_DivideT, (T_Identifier "ff"), T_ElseT, T_ReturnT, T_LParenT, (T_Identifier "x"), T_MinusToken, (T_Identifier "ff"), T_DivideT, (T_IntDigit "2"), T_RParenT, T_DivideT, (T_Identifier "ff"), T_FiT, T_EndT, T_IntT, (T_Identifier "add"), T_LParenT, T_IntT, (T_Identifier "x1"), T_CoT, T_IntT, (T_Identifier "x2"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "x1"), T_PlusToken, (T_Identifier "x2"), T_EndT, T_IntT, (T_Identifier "subtract"), T_LParenT, T_IntT, (T_Identifier "x1"), T_CoT, T_IntT, (T_Identifier "x2"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "x1"), T_MinusToken, (T_Identifier "x2"), T_EndT, T_IntT, (T_Identifier "addByInt"), T_LParenT, T_IntT, (T_Identifier "x"), T_CoT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "x"), T_PlusToken, (T_Identifier "n"), T_TimesT, (T_Identifier "ff"), T_EndT, T_IntT, (T_Identifier "subtractByInt"), T_LParenT, T_IntT, (T_Identifier "x"), T_CoT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "x"), T_MinusToken, (T_Identifier "n"), T_TimesT, (T_Identifier "ff"), T_EndT, T_IntT, (T_Identifier "multiply"), T_LParenT, T_IntT, (T_Identifier "x1"), T_CoT, T_IntT, (T_Identifier "x2"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "x1"), T_TimesT, (T_Identifier "x2"), T_DivideT, (T_Identifier "ff"), T_EndT, T_IntT, (T_Identifier "multiplyByInt"), T_LParenT, T_IntT, (T_Identifier "x"), T_CoT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "x"), T_TimesT, (T_Identifier "n"), T_EndT, T_IntT, (T_Identifier "divide"), T_LParenT, T_IntT, (T_Identifier "x1"), T_CoT, T_IntT, (T_Identifier "x2"), T_RParenT, T_IsT, T_IntT, (T_Identifier "ff"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_ReturnT, (T_Identifier "x1"), T_TimesT, (T_Identifier "ff"), T_DivideT, (T_Identifier "x2"), T_EndT, T_IntT, (T_Identifier "divideByInt"), T_LParenT, T_IntT, (T_Identifier "x"), T_CoT, T_IntT, (T_Identifier "n"), T_RParenT, T_IsT, T_ReturnT, (T_Identifier "x"), T_DivideT, (T_Identifier "n"), T_EndT, T_IntT, (T_Identifier "n1"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "n2"), T_EqualT, (T_IntDigit "3"), T_SepT, T_PrintT, (T_StringLiteral "\"Using fixed-point real: \""), T_SepT, T_PrintT, (T_Identifier "n1"), T_SepT, T_PrintT, (T_StringLiteral "\" / \""), T_SepT, T_PrintT, (T_Identifier "n2"), T_SepT, T_PrintT, (T_StringLiteral "\" * \""), T_SepT, T_PrintT, (T_Identifier "n2"), T_SepT, T_PrintT, (T_StringLiteral "\" = \""), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "intToFixedPoint"), T_LParenT, (T_Identifier "n1"), T_RParenT, T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "divideByInt"), T_LParenT, (T_Identifier "x"), T_CoT, (T_Identifier "n2"), T_RParenT, T_SepT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "multiplyByInt"), T_LParenT, (T_Identifier "x"), T_CoT, (T_Identifier "n2"), T_RParenT, T_SepT, T_IntT, (T_Identifier "result"), T_EqualT, T_CallT, (T_Identifier "fixedPointToIntRoundNear"), T_LParenT, (T_Identifier "x"), T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "result"), T_EndT]

functionConditionalReturn :: Assertion
functionConditionalReturn = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/function/nested_functions/functionConditionalReturn.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "f"), T_LParenT, T_RParenT, T_IsT, T_IfT, T_TrueToken, T_ThenT, T_ReturnT, T_TrueToken, T_ElseT, T_ReturnT, T_FalseToken, T_FiT, T_EndT, T_BoolT, (T_Identifier "x"), T_EqualT, T_CallT, (T_Identifier "f"), T_LParenT, T_RParenT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

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
if1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if1.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IfT, (T_Identifier "a"), T_EqT, (T_IntDigit "13"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_EndT]

if2 :: Assertion
if2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IfT, (T_Identifier "a"), T_NotEqT, (T_IntDigit "13"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_FiT, T_EndT]

if3 :: Assertion
if3 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if3.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IntT, (T_Identifier "b"), T_EqualT, (T_IntDigit "37"), T_SepT, T_IfT, (T_Identifier "a"), T_LessT, (T_Identifier "b"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_EndT]

if4 :: Assertion
if4 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if4.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "c"), T_EqualT, T_FalseToken, T_SepT, T_IfT, (T_Identifier "b"), T_AndT, (T_Identifier "c"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_FiT, T_EndT]

if5 :: Assertion
if5 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if5.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_SepT, T_BoolT, (T_Identifier "c"), T_EqualT, T_FalseToken, T_SepT, T_IfT, (T_Identifier "b"), T_OrT, (T_Identifier "c"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_EndT]

if6 :: Assertion
if6 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/if6.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c1"), T_EqualT, (T_CharLiteral "'f'"), T_SepT, T_CharT, (T_Identifier "c2"), T_EqualT, (T_CharLiteral "'F'"), T_SepT, T_IfT, (T_Identifier "c1"), T_EqT, (T_Identifier "c2"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_FiT, T_EndT]

ifBasic :: Assertion
ifBasic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/ifBasic.wacc" ))
  @=? [ T_BeginT, T_IfT, T_TrueToken, T_ThenT, T_SkipT, T_ElseT, T_SkipT, T_FiT, T_EndT]

ifFalse :: Assertion
ifFalse = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/ifFalse.wacc" ))
  @=? [ T_BeginT, T_IfT, T_FalseToken, T_ThenT, T_PrintLnT, (T_StringLiteral "\"not here\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"here\""), T_FiT, T_EndT]

ifTrue :: Assertion
ifTrue = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/ifTrue.wacc" ))
  @=? [ T_BeginT, T_IfT, T_TrueToken, T_ThenT, T_PrintLnT, (T_StringLiteral "\"here\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"not here\""), T_FiT, T_EndT]

whitespaceIf :: Assertion
whitespaceIf = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/if/whitespace.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IfT, (T_Identifier "a"), T_EqT, (T_IntDigit "13"), T_ThenT, (T_Identifier "a"), T_EqualT, (T_IntDigit "1"), T_ElseT, (T_Identifier "a"), T_EqualT, (T_IntDigit "0"), T_FiT, T_SepT, T_PrintLnT, (T_Identifier "a"), T_EndT]
     
ioTests :: [TestTree]
ioTests =
  [ testCase "IO Loop" ioLoop
  , testCase "IO Sequence" ioSequence
  , testGroup "Print Tests" printTests
  , testGroup "Read Tests" readTests
  ]

ioLoop :: Assertion
ioLoop = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/IOLoop.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "continue"), T_EqualT, (T_CharLiteral "'Y'"), T_SepT, T_IntT, (T_Identifier "buff"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "continue"), T_NotEqT, (T_CharLiteral "'N'"), T_DoT, T_PrintT, (T_StringLiteral "\"Please input an integer: \""), T_SepT, T_ReadT, (T_Identifier "buff"), T_SepT, T_PrintT, (T_StringLiteral "\"echo input: \""), T_SepT, T_PrintLnT, (T_Identifier "buff"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Do you want to continue entering input?\""), T_SepT, T_PrintLnT, (T_StringLiteral "\"(enter Y for \\'yes\\' and N for \\'no\\')\""), T_SepT, T_ReadT, (T_Identifier "continue"), T_DoneT, T_EndT] 

ioSequence :: Assertion
ioSequence = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/IOSequence.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_StringLiteral "\"Please input an integer: \""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintT, (T_StringLiteral "\"You input: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

printTests :: [TestTree]
printTests = 
  [ testCase "Multiple String Assignments" multipleStringAssignments
  , testCase "Hash in program" hashInProgram
  , testCase "Print" printT
  , testCase "Print Bool" printBool
  , testCase "Print Char" printChar
  , testCase "Print Esc Char" printEscChar
  , testCase "Print Int" printInt
  , testCase "Print Ln" println
  ]

hashInProgram :: Assertion
hashInProgram = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/hashInProgram.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_StringLiteral "\"We can print the hash character: \""), T_SepT, T_PrintLnT, (T_CharLiteral "'#'"), T_SepT, T_PrintLnT, (T_StringLiteral "\"We can also print # when its in a string.\""), T_EndT] 

multipleStringAssignments :: Assertion
multipleStringAssignments = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/multipleStringsAssignment.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s1"), T_EqualT, (T_StringLiteral "\"Hi\""), T_SepT, T_StringT, (T_Identifier "s2"), T_EqualT, (T_StringLiteral "\"Hi\""), T_SepT, T_PrintT, (T_StringLiteral "\"s1 is \""), T_SepT, T_PrintLnT, (T_Identifier "s1"), T_SepT, T_PrintT, (T_StringLiteral "\"s2 is \""), T_SepT, T_PrintLnT, (T_Identifier "s2"), T_SepT, T_IfT, (T_Identifier "s1"), T_EqT, (T_Identifier "s2"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"They are the same.\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"They are not the same.\""), T_FiT, T_SepT, T_PrintLnT, (T_StringLiteral "\"Now modify s1[0] = \\'h\\'\""), T_SepT, (T_Identifier "s1"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_EqualT, (T_CharLiteral "'h'"), T_SepT, T_PrintT, (T_StringLiteral "\"s1 is \""), T_SepT, T_PrintLnT, (T_Identifier "s1"), T_SepT, T_PrintT, (T_StringLiteral "\"s2 is \""), T_SepT, T_PrintLnT, (T_Identifier "s2"), T_SepT, T_IfT, (T_Identifier "s1"), T_EqT, (T_Identifier "s2"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"They are the same.\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"They are not the same.\""), T_FiT, T_EndT]

printT :: Assertion
printT = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/print.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"Hello World!\\n\""), T_EndT]

printBool :: Assertion
printBool = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/printBool.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"True is \""), T_SepT, T_PrintLnT, T_TrueToken, T_SepT, T_PrintT, (T_StringLiteral "\"False is \""), T_SepT, T_PrintLnT, T_FalseToken, T_EndT]

printChar :: Assertion
printChar = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/printChar.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"A simple character example is \""), T_SepT, T_PrintLnT, (T_CharLiteral "'f'"), T_EndT]

printEscChar :: Assertion
printEscChar = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/printEscChar.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"An escaped character example is \""), T_SepT, T_PrintLnT, (T_CharLiteral "'\\\"'"), T_EndT]

printInt :: Assertion
printInt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/printInt.wacc" ))
  @=? [ T_BeginT, T_PrintT, (T_StringLiteral "\"An example integer is \""), T_SepT, T_PrintLnT, (T_IntDigit "189"), T_EndT]

println :: Assertion
println = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/print/println.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_StringLiteral "\"Hello World!\""), T_EndT]


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
echoBigInt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoBigInt.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter an integer to echo\""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

echoBigNegInt :: Assertion
echoBigNegInt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoBigNegInt.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter an integer to echo\""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

echoChar :: Assertion
echoChar = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoChar.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter a character to echo\""), T_SepT, T_ReadT, (T_Identifier "c"), T_SepT, T_PrintLnT, (T_Identifier "c"), T_EndT]

echoInt :: Assertion
echoInt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoInt.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter an integer to echo\""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

echoNegInt :: Assertion
echoNegInt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoNegInt.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter an integer to echo\""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

echoPuncChar :: Assertion
echoPuncChar = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/echoPuncChar.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_PrintLnT, (T_StringLiteral "\"enter a character to echo\""), T_SepT, T_ReadT, (T_Identifier "c"), T_SepT, T_PrintLnT, (T_Identifier "c"), T_EndT]

readT :: Assertion
readT = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/IO/read/read.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, T_PrintLnT, (T_StringLiteral "\"input an integer to continue...\""), T_SepT, T_ReadT, (T_Identifier "x"), T_EndT]

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
checkRefPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/checkRefPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "q"), T_EqualT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "q"), T_SepT, T_PrintLnT, (T_Identifier "p"), T_EqT, (T_Identifier "q"), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_FstT, (T_Identifier "q"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EqT, (T_Identifier "y"), T_SepT, T_CharT, (T_Identifier "c1"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_CharT, (T_Identifier "c2"), T_EqualT, T_SndT, (T_Identifier "q"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_SepT, T_PrintLnT, (T_Identifier "c2"), T_SepT, T_PrintLnT, (T_Identifier "c1"), T_EqT, (T_Identifier "c2"), T_EndT]

createPair :: Assertion
createPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/createPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_IntDigit "3"), T_RParenT, T_EndT]

createPair02 :: Assertion
createPair02 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/createPair02.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_CharT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_CharLiteral "'a'"), T_CoT, (T_CharLiteral "'b'"), T_RParenT, T_EndT]

createPair03 :: Assertion
createPair03 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/createPair03.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_EndT]

createRefPair :: Assertion
createRefPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/createRefPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "q"), T_EqualT, (T_Identifier "p"), T_EndT]

freePair :: Assertion
freePair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/free.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "a"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "a"), T_EndT]

linkedList :: Assertion
linkedList = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/linkedList.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "11"), T_CoT, T_NullT, T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "q"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "4"), T_CoT, (T_Identifier "p"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "r"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "2"), T_CoT, (T_Identifier "q"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "s"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "1"), T_CoT, (T_Identifier "r"), T_RParenT, T_SepT, T_PrintT, (T_StringLiteral "\"list = {\""), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "x"), T_EqualT, (T_Identifier "s"), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "y"), T_EqualT, T_SndT, (T_Identifier "x"), T_SepT, T_IntT, (T_Identifier "f"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "y"), T_NotEqT, T_NullT, T_DoT, (T_Identifier "f"), T_EqualT, T_FstT, (T_Identifier "x"), T_SepT, T_PrintT, (T_Identifier "f"), T_SepT, T_PrintT, (T_StringLiteral "\", \""), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "y"), T_SepT, (T_Identifier "y"), T_EqualT, T_SndT, (T_Identifier "x"), T_DoneT, T_SepT, (T_Identifier "f"), T_EqualT, T_FstT, (T_Identifier "x"), T_SepT, T_PrintT, (T_Identifier "f"), T_SepT, T_PrintLnT, (T_StringLiteral "\"}\""), T_EndT]

nestedPair :: Assertion
nestedPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/nestedPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_PairT, T_RParenT, (T_Identifier "q"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "1"), T_CoT, (T_Identifier "p"), T_RParenT, T_EndT]

nullPair :: Assertion
nullPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/null.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_PrintLnT, (T_Identifier "p"), T_SepT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_PrintLnT, (T_Identifier "p"), T_EndT]

printNullP :: Assertion
printNullP = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/printNull.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, T_NullT, T_EndT]

printNullPair :: Assertion
printNullPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/printNullPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_PrintLnT, (T_Identifier "p"), T_EndT]

printPair :: Assertion
printPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/printPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_PrintT, (T_Identifier "p"), T_SepT, T_PrintT, (T_StringLiteral "\" = (\""), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintT, (T_Identifier "x"), T_SepT, T_PrintT, (T_StringLiteral "\", \""), T_SepT, T_CharT, (T_Identifier "c"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_PrintT, (T_Identifier "c"), T_SepT, T_PrintLnT, (T_CharLiteral "')'"), T_EndT]

printPairOfNulls :: Assertion
printPairOfNulls = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/printPairOfNulls.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, T_NullT, T_CoT, T_NullT, T_RParenT, T_SepT, T_PrintT, (T_Identifier "p"), T_SepT, T_PrintT, (T_StringLiteral "\" = (\""), T_SepT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "q"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintT, (T_Identifier "q"), T_SepT, T_PrintT, (T_StringLiteral "\",\""), T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_BoolT, T_RParenT, (T_Identifier "r"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_PrintT, (T_Identifier "r"), T_SepT, T_PrintLnT, (T_StringLiteral "\")\""), T_EndT]

readPair :: Assertion
readPair = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/readPair.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_CharT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_CharLiteral "'\\0'"), T_CoT, (T_IntDigit "0"), T_RParenT, T_SepT, T_PrintT, (T_StringLiteral "\"Please enter the first element (char): \""), T_SepT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'0'"), T_SepT, T_ReadT, (T_Identifier "c"), T_SepT, T_FstT, (T_Identifier "p"), T_EqualT, (T_Identifier "c"), T_SepT, T_PrintT, (T_StringLiteral "\"Please enter the second element (int): \""), T_SepT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_ReadT, (T_Identifier "i"), T_SepT, T_SndT, (T_Identifier "p"), T_EqualT, (T_Identifier "i"), T_SepT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, (T_Identifier "i"), T_EqualT, T_MinusToken, (T_IntDigit "1"), T_SepT, T_PrintT, (T_StringLiteral "\"The first element was \""), T_SepT, (T_Identifier "c"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "c"), T_SepT, T_PrintT, (T_StringLiteral "\"The second element was \""), T_SepT, (T_Identifier "i"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "i"), T_EndT]

writeFst :: Assertion
writeFst = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/writeFst.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_IntT, (T_Identifier "f"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "f"), T_SepT, T_FstT, (T_Identifier "p"), T_EqualT, (T_IntDigit "42"), T_SepT, (T_Identifier "f"), T_EqualT, T_FstT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "f"), T_EndT]

writeSnd :: Assertion
writeSnd = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/pairs/writeSnd.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_CharT, (T_Identifier "s"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "s"), T_SepT, T_SndT, (T_Identifier "p"), T_EqualT, (T_CharLiteral "'Z'"), T_SepT, (T_Identifier "s"), T_EqualT, T_SndT, (T_Identifier "p"), T_SepT, T_PrintLnT, (T_Identifier "s"), T_EndT]

     
runtimeErrTests :: [TestTree]
runtimeErrTests = 
  [ testGroup "Array out of bound" arrayOutOfBound
  , testGroup "Devide by zero" divideByZerogp
  , testGroup "Double frees" doubleFrees
  , testGroup "Integer Overflow" integerOverflow
  , testGroup "Null Dereference" nullDereference
  ]

arrayOutOfBound :: [TestTree]
arrayOutOfBound = 
  [ testCase "Array neg Bound" arrayNegBounds
  , testCase "Array out of bounds" arrayOutOfBounds
  , testCase "Array out of bound" arrayOutOfBoundsWrite
  ]

arrayNegBounds :: Assertion
arrayNegBounds = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "43"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "18"), T_CoT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "b"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "a"), T_LBracketT, T_MinusToken, (T_IntDigit "2"), T_RBracketT, T_EndT]

arrayOutOfBounds :: Assertion
arrayOutOfBounds = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "b"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "43"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "18"), T_CoT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintLnT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "5"), T_RBracketT, T_EndT]

arrayOutOfBoundsWrite :: Assertion
arrayOutOfBoundsWrite = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc" ))
  @=? [ T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "b"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_LBracketT, (T_IntDigit "43"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "18"), T_CoT, (T_IntDigit "1"), T_RBracketT, T_SepT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "5"), T_RBracketT, T_EqualT, (T_IntDigit "100"), T_SepT, T_PrintLnT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "5"), T_RBracketT, T_EndT]

divideByZerogp :: [TestTree]
divideByZerogp = 
  [ testCase "Div Zero 1" divZero
  , testCase "Div Zero 2" divideByZero
  , testCase "Mod By Zero" modByZero
  ]

divZero :: Assertion
divZero = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/divideByZero/divZero.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_DivideT, (T_IntDigit "0"), T_SepT, T_PrintLnT, (T_StringLiteral "\"should not reach here\""), T_EndT]

divideByZero :: Assertion
divideByZero = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/divideByZero/divideByZero.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_Identifier "x"), T_DivideT, (T_Identifier "y"), T_EndT]

modByZero :: Assertion
modByZero = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/divideByZero/modByZero.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_Identifier "x"), T_ModuloT, (T_Identifier "y"), T_EndT]

doubleFrees :: [TestTree]
doubleFrees = 
  [ testCase "Double free" doubleFree
  , testCase "Hidden double free" hiddenDoubleFree
  ]

doubleFree :: Assertion
doubleFree = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/doubleFrees/doubleFree.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "a"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_FreeT, (T_Identifier "a"), T_SepT, T_FreeT, (T_Identifier "a"), T_EndT]

hiddenDoubleFree :: Assertion
hiddenDoubleFree = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/doubleFrees/hiddenDoubleFree.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "a"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "10"), T_CoT, (T_CharLiteral "'a'"), T_RParenT, T_SepT, T_PairT, T_LParenT, T_IntT, T_CoT, T_CharT, T_RParenT, (T_Identifier "b"), T_EqualT, (T_Identifier "a"), T_SepT, T_FreeT, (T_Identifier "a"), T_SepT, T_FreeT, (T_Identifier "b"), T_EndT]

integerOverflow :: [TestTree]
integerOverflow = 
  [ testCase "Int Just Overflow" intJustOverflow
  , testCase "Int Underflow" intUnderflow
  , testCase "Int way Overflow" intWayOverflow
  , testCase "Int Mul Overflow" intmultOverflow
  , testCase "Int negate Overflow 1" intnegateOverflow
  , testCase "Int negate Overflow 2" intnegateOverflow2
  , testCase "Int negate Overflow 3" intnegateOverflow3
  , testCase "Int negate Overflow 4" intnegateOverflow4
  ]

intJustOverflow :: Assertion
intJustOverflow = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intJustOverflow.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2147483646"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intUnderflow :: Assertion
intUnderflow = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intUnderflow.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "2147483647"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_MinusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_MinusToken, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intWayOverflow :: Assertion
intWayOverflow = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intWayOverflow.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2000000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "2000000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intmultOverflow :: Assertion
intmultOverflow = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intmultOverflow.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2147483"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_TimesT, (T_IntDigit "1000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_TimesT, (T_IntDigit "1000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_TimesT, (T_IntDigit "1000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intnegateOverflow :: Assertion
intnegateOverflow = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "2147483648"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intnegateOverflow2 :: Assertion
intnegateOverflow2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "2147483648"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_TimesT, (T_IntDigit "10"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]
intnegateOverflow3 :: Assertion
intnegateOverflow3 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "20000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_TimesT, (T_IntDigit "100000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

intnegateOverflow4 :: Assertion
intnegateOverflow4 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "2000000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_MinusToken, (T_IntDigit "2000000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

nullDereference :: [TestTree]
nullDereference =
  [ testCase "Free Null" freeNull
  , testCase "Read Null 1" readNull
  , testCase "Read Null 2" readNull2
  , testCase "Set Null 1" setNull1
  , testCase "Set Null 2" setNull2
  , testCase "Use Null 1" useNull1
  , testCase "Use Null 2" useNull2
  ]

freeNull :: Assertion
freeNull = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/freeNull.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_PairT, T_CoT, T_PairT, T_RParenT, (T_Identifier "a"), T_EqualT, T_NullT, T_SepT, T_FreeT, (T_Identifier "a"), T_EndT]

readNull :: Assertion
readNull = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/readNull1.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_ReadT, T_FstT, (T_Identifier "p"), T_EndT]

readNull2 :: Assertion
readNull2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/readNull2.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_ReadT, T_SndT, (T_Identifier "p"), T_EndT]

setNull1 :: Assertion
setNull1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/setNull1.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_FstT, (T_Identifier "p"), T_EqualT, (T_IntDigit "1"), T_EndT]

setNull2 :: Assertion
setNull2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/setNull2.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_SndT, (T_Identifier "p"), T_EqualT, (T_IntDigit "1"), T_EndT]

useNull1 :: Assertion
useNull1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/useNull1.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_FstT, (T_Identifier "p"), T_EndT]

useNull2 :: Assertion
useNull2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/runtimeErr/nullDereference/useNull2.wacc" ))
  @=? [ T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "p"), T_EqualT, T_NullT, T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, T_SndT, (T_Identifier "p"), T_EndT]


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
ifNested1 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/ifNested1.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IfT, (T_Identifier "a"), T_EqT, (T_IntDigit "13"), T_ThenT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_IntDigit "5"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_EndT]

ifNested2 :: Assertion
ifNested2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/ifNested2.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "a"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IfT, (T_Identifier "a"), T_EqT, (T_IntDigit "13"), T_ThenT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_IntDigit "5"), T_ThenT, T_IfT, (T_Identifier "a"), T_LessT, (T_IntDigit "10"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_ElseT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_IntDigit "12"), T_ThenT, T_IfT, (T_Identifier "a"), T_GreaterT, (T_IntDigit "13"), T_ThenT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_ElseT, T_PrintLnT, (T_StringLiteral "\"correct\""), T_FiT, T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_FiT, T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_ElseT, T_PrintLnT, (T_StringLiteral "\"incorrect\""), T_FiT, T_EndT]

indentationNotImportant :: Assertion
indentationNotImportant = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/indentationNotImportant.wacc" ))
  @=? [ T_BeginT, T_WhileT, T_FalseToken, T_DoT, T_SkipT, T_DoneT, T_EndT]

intsAndKeywords :: Assertion
intsAndKeywords = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/intsAndKeywords.wacc" ))
  @=? [ T_BeginT, T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "125"), T_EndT, T_EndT]

printAllTypes :: Assertion
printAllTypes = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/printAllTypes.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "comma"), T_EqualT, (T_StringLiteral "\", \""), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "5"), T_SepT, T_BeginT, T_CharT, (T_Identifier "x"), T_EqualT, (T_CharLiteral "'x'"), T_SepT, T_BeginT, T_BoolT, (T_Identifier "x"), T_EqualT, T_TrueToken, T_SepT, T_BeginT, T_StringT, (T_Identifier "x"), T_EqualT, (T_StringLiteral "\"this is a string\""), T_SepT, T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "x"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_BeginT, T_CharT, T_LBracketT, T_RBracketT, (T_Identifier "x"), T_EqualT, T_LBracketT, (T_CharLiteral "'x'"), T_CoT, (T_CharLiteral "'y'"), T_CoT, (T_CharLiteral "'z'"), T_RBracketT, T_SepT, T_BeginT, T_BoolT, T_LBracketT, T_RBracketT, (T_Identifier "x"), T_EqualT, T_LBracketT, T_TrueToken, T_CoT, T_FalseToken, T_CoT, T_TrueToken, T_RBracketT, T_SepT, T_BeginT, T_StringT, T_LBracketT, T_RBracketT, (T_Identifier "x"), T_EqualT, T_LBracketT, (T_StringLiteral "\"array\""), T_CoT, (T_StringLiteral "\"of\""), T_CoT, (T_StringLiteral "\"strings\""), T_RBracketT, T_SepT, T_BeginT, T_PairT, T_LParenT, T_IntT, T_CoT, T_IntT, T_RParenT, (T_Identifier "x"), T_EqualT, T_NewpairT, T_LParenT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_RParenT, T_SepT, T_BeginT, T_PairT, T_LParenT, T_CharT, T_CoT, T_BoolT, T_RParenT, (T_Identifier "y"), T_EqualT, T_NewpairT, T_LParenT, (T_CharLiteral "'a'"), T_CoT, T_TrueToken, T_RParenT, T_SepT, T_PairT, T_LParenT, T_CharT, T_CoT, T_BoolT, T_RParenT, (T_Identifier "z"), T_EqualT, T_NewpairT, T_LParenT, (T_CharLiteral "'b'"), T_CoT, T_FalseToken, T_RParenT, T_SepT, T_PairT, T_LParenT, T_CharT, T_CoT, T_BoolT, T_RParenT, T_LBracketT, T_RBracketT, (T_Identifier "x"), T_EqualT, T_LBracketT, (T_Identifier "y"), T_CoT, (T_Identifier "z"), T_RBracketT, T_SepT, T_BeginT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "y"), T_EqualT, T_LBracketT, (T_IntDigit "1"), T_CoT, (T_IntDigit "2"), T_CoT, (T_IntDigit "3"), T_RBracketT, T_SepT, T_CharT, T_LBracketT, T_RBracketT, (T_Identifier "z"), T_EqualT, T_LBracketT, (T_CharLiteral "'a'"), T_CoT, (T_CharLiteral "'b'"), T_CoT, (T_CharLiteral "'c'"), T_RBracketT, T_SepT, T_PairT, T_LParenT, T_IntT, T_LBracketT, T_RBracketT, T_CoT, T_CharT, T_LBracketT, T_RBracketT, T_RParenT, (T_Identifier "x"), T_EqualT, T_NewpairT, T_LParenT, (T_Identifier "y"), T_CoT, (T_Identifier "z"), T_RParenT, T_SepT, T_BeginT, T_SkipT, T_EndT, T_SepT, T_IntT, T_LBracketT, T_RBracketT, (T_Identifier "a"), T_EqualT, T_FstT, (T_Identifier "x"), T_SepT, T_CharT, T_LBracketT, T_RBracketT, (T_Identifier "b"), T_EqualT, T_SndT, (T_Identifier "x"), T_SepT, T_PrintT, (T_StringLiteral "\"( [\""), T_SepT, T_PrintT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "a"), T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_PrintT, (T_StringLiteral "\"] , [\""), T_SepT, T_PrintT, (T_Identifier "b"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "b"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "b"), T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_PrintLnT, (T_StringLiteral "\"] )\""), T_EndT, T_SepT, T_PairT, T_LParenT, T_CharT, T_CoT, T_BoolT, T_RParenT, (T_Identifier "a"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_CharT, (T_Identifier "aa"), T_EqualT, T_FstT, (T_Identifier "a"), T_SepT, T_BoolT, (T_Identifier "ab"), T_EqualT, T_SndT, (T_Identifier "a"), T_SepT, T_PairT, T_LParenT, T_CharT, T_CoT, T_BoolT, T_RParenT, (T_Identifier "b"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_CharT, (T_Identifier "ba"), T_EqualT, T_FstT, (T_Identifier "b"), T_SepT, T_BoolT, (T_Identifier "bb"), T_EqualT, T_SndT, (T_Identifier "b"), T_SepT, T_PrintT, (T_StringLiteral "\"[ \""), T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_StringLiteral "\" = (\""), T_SepT, T_PrintT, (T_Identifier "aa"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "ab"), T_SepT, T_PrintT, (T_StringLiteral "\"), \""), T_SepT, T_PrintT, (T_Identifier "b"), T_SepT, T_PrintT, (T_StringLiteral "\" = (\""), T_SepT, T_PrintT, (T_Identifier "ba"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "bb"), T_SepT, T_PrintLnT, (T_StringLiteral "\") ]\""), T_EndT, T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, T_FstT, (T_Identifier "x"), T_SepT, T_IntT, (T_Identifier "z"), T_EqualT, T_SndT, (T_Identifier "x"), T_SepT, T_PrintT, (T_Identifier "y"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintLnT, (T_Identifier "z"), T_EndT, T_SepT, T_StringT, (T_Identifier "a"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_StringT, (T_Identifier "b"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_StringT, (T_Identifier "c"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "b"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintLnT, (T_Identifier "c"), T_EndT, T_SepT, T_PrintT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_IntT, (T_Identifier "a"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "0"), T_RBracketT, T_SepT, T_IntT, (T_Identifier "b"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "1"), T_RBracketT, T_SepT, T_IntT, (T_Identifier "c"), T_EqualT, (T_Identifier "x"), T_LBracketT, (T_IntDigit "2"), T_RBracketT, T_SepT, T_PrintT, (T_Identifier "a"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintT, (T_Identifier "b"), T_SepT, T_PrintT, (T_Identifier "comma"), T_SepT, T_PrintLnT, (T_Identifier "c"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

scope :: Assertion
scope = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/scope.wacc" ))
  @=? [ T_BeginT, T_BeginT, T_BeginT, T_BeginT, T_BeginT, T_SkipT, T_EndT, T_EndT, T_EndT, T_EndT, T_EndT]

scopeBasic :: Assertion
scopeBasic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/scopeBasic.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_SepT, T_BeginT, T_SkipT, T_EndT, T_EndT]

scopeRedefine :: Assertion
scopeRedefine = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/scopeRedefine.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "1"), T_SepT, T_BeginT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_BoolT, (T_Identifier "x"), T_EqualT, T_TrueToken, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

scopeSimpleRedefine :: Assertion
scopeSimpleRedefine = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/scopeSimpleRedefine.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "12"), T_SepT, T_BeginT, T_BoolT, (T_Identifier "x"), T_EqualT, T_TrueToken, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

scopeVars :: Assertion
scopeVars = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/scope/scopeVars.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "2"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "4"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT, T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

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
basicSeq = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/basicSeq.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_SepT, T_SkipT, T_EndT]

basicSeq2 :: Assertion
basicSeq2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/basicSeq2.wacc" ))
  @=? [ T_BeginT, T_SkipT, T_SepT, T_SkipT, T_SepT, T_SkipT, T_EndT]

boolAssignment :: Assertion
boolAssignment = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/boolAssignment.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_FalseToken, T_SepT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_EndT]

charAssignment :: Assertion
charAssignment = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/charAssignment.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'a'"), T_SepT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'Z'"), T_EndT]

exitSimple :: Assertion
exitSimple = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/exitSimple.wacc" ))
  @=? [ T_BeginT, T_ExitT, (T_IntDigit "42"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Should not print this.\""), T_EndT]

intAssignment :: Assertion
intAssignment = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/intAssignment.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, (T_Identifier "x"), T_EqualT, (T_IntDigit "20"), T_SepT, T_ExitT, (T_Identifier "x"), T_EndT]

intLeadingZeros :: Assertion
intLeadingZeros = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/intLeadingZeros.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0000000000000000000000000000000000000042"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "0000000000000000000000000000000000000000"), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_PrintLnT, (T_Identifier "y"), T_EndT]

stringAssignment :: Assertion
stringAssignment = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/sequence/stringAssignment.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"foo\""), T_SepT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"bar\""), T_EndT]

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
underscoreVarName = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/_VarNames.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "an_underscore"), T_EqualT, (T_IntDigit "19"), T_SepT, T_ExitT, (T_Identifier "an_underscore"), T_EndT]

boolDeclaration :: Assertion
boolDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/boolDeclaration.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_FalseToken, T_EndT]

boolDeclaration2 :: Assertion
boolDeclaration2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/boolDeclaration2.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_EndT]

capCharDeclaration :: Assertion
capCharDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/capCharDeclaration.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'M'"), T_EndT]

charDeclaration :: Assertion
charDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/charDeclaration.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'a'"), T_EndT]

charDeclaration2 :: Assertion
charDeclaration2 = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/charDeclaration2.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'z'"), T_EndT]

emptyStringDeclaration :: Assertion
emptyStringDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/emptyStringDeclaration.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"\""), T_EndT]

intDeclaration :: Assertion
intDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/intDeclaration.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "42"), T_EndT]

longVarNames :: Assertion
longVarNames = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/longVarNames.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid"), T_EqualT, (T_IntDigit "5"), T_SepT, T_ExitT, (T_Identifier "this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid"), T_EndT]

manyVariables :: Assertion
manyVariables = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/manyVariables.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x0"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "x1"), T_EqualT, (T_IntDigit "1"), T_SepT, T_IntT, (T_Identifier "x2"), T_EqualT, (T_IntDigit "2"), T_SepT, T_IntT, (T_Identifier "x3"), T_EqualT, (T_IntDigit "3"), T_SepT, T_IntT, (T_Identifier "x4"), T_EqualT, (T_IntDigit "4"), T_SepT, T_IntT, (T_Identifier "x5"), T_EqualT, (T_IntDigit "5"), T_SepT, T_IntT, (T_Identifier "x6"), T_EqualT, (T_IntDigit "6"), T_SepT, T_IntT, (T_Identifier "x7"), T_EqualT, (T_IntDigit "7"), T_SepT, T_IntT, (T_Identifier "x8"), T_EqualT, (T_IntDigit "8"), T_SepT, T_IntT, (T_Identifier "x9"), T_EqualT, (T_IntDigit "9"), T_SepT, T_IntT, (T_Identifier "x10"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "x11"), T_EqualT, (T_IntDigit "11"), T_SepT, T_IntT, (T_Identifier "x12"), T_EqualT, (T_IntDigit "12"), T_SepT, T_IntT, (T_Identifier "x13"), T_EqualT, (T_IntDigit "13"), T_SepT, T_IntT, (T_Identifier "x14"), T_EqualT, (T_IntDigit "14"), T_SepT, T_IntT, (T_Identifier "x15"), T_EqualT, (T_IntDigit "15"), T_SepT, T_IntT, (T_Identifier "x16"), T_EqualT, (T_IntDigit "16"), T_SepT, T_IntT, (T_Identifier "x17"), T_EqualT, (T_IntDigit "17"), T_SepT, T_IntT, (T_Identifier "x18"), T_EqualT, (T_IntDigit "18"), T_SepT, T_IntT, (T_Identifier "x19"), T_EqualT, (T_IntDigit "19"), T_SepT, T_IntT, (T_Identifier "x20"), T_EqualT, (T_IntDigit "20"), T_SepT, T_IntT, (T_Identifier "x21"), T_EqualT, (T_IntDigit "21"), T_SepT, T_IntT, (T_Identifier "x22"), T_EqualT, (T_IntDigit "22"), T_SepT, T_IntT, (T_Identifier "x23"), T_EqualT, (T_IntDigit "23"), T_SepT, T_IntT, (T_Identifier "x24"), T_EqualT, (T_IntDigit "24"), T_SepT, T_IntT, (T_Identifier "x25"), T_EqualT, (T_IntDigit "25"), T_SepT, T_IntT, (T_Identifier "x26"), T_EqualT, (T_IntDigit "26"), T_SepT, T_IntT, (T_Identifier "x27"), T_EqualT, (T_IntDigit "27"), T_SepT, T_IntT, (T_Identifier "x28"), T_EqualT, (T_IntDigit "28"), T_SepT, T_IntT, (T_Identifier "x29"), T_EqualT, (T_IntDigit "29"), T_SepT, T_IntT, (T_Identifier "x30"), T_EqualT, (T_IntDigit "30"), T_SepT, T_IntT, (T_Identifier "x31"), T_EqualT, (T_IntDigit "31"), T_SepT, T_IntT, (T_Identifier "x32"), T_EqualT, (T_IntDigit "32"), T_SepT, T_IntT, (T_Identifier "x33"), T_EqualT, (T_IntDigit "33"), T_SepT, T_IntT, (T_Identifier "x34"), T_EqualT, (T_IntDigit "34"), T_SepT, T_IntT, (T_Identifier "x35"), T_EqualT, (T_IntDigit "35"), T_SepT, T_IntT, (T_Identifier "x36"), T_EqualT, (T_IntDigit "36"), T_SepT, T_IntT, (T_Identifier "x37"), T_EqualT, (T_IntDigit "37"), T_SepT, T_IntT, (T_Identifier "x38"), T_EqualT, (T_IntDigit "38"), T_SepT, T_IntT, (T_Identifier "x39"), T_EqualT, (T_IntDigit "39"), T_SepT, T_IntT, (T_Identifier "x40"), T_EqualT, (T_IntDigit "40"), T_SepT, T_IntT, (T_Identifier "x41"), T_EqualT, (T_IntDigit "41"), T_SepT, T_IntT, (T_Identifier "x42"), T_EqualT, (T_IntDigit "42"), T_SepT, T_IntT, (T_Identifier "x43"), T_EqualT, (T_IntDigit "43"), T_SepT, T_IntT, (T_Identifier "x44"), T_EqualT, (T_IntDigit "44"), T_SepT, T_IntT, (T_Identifier "x45"), T_EqualT, (T_IntDigit "45"), T_SepT, T_IntT, (T_Identifier "x46"), T_EqualT, (T_IntDigit "46"), T_SepT, T_IntT, (T_Identifier "x47"), T_EqualT, (T_IntDigit "47"), T_SepT, T_IntT, (T_Identifier "x48"), T_EqualT, (T_IntDigit "48"), T_SepT, T_IntT, (T_Identifier "x49"), T_EqualT, (T_IntDigit "49"), T_SepT, T_IntT, (T_Identifier "x50"), T_EqualT, (T_IntDigit "50"), T_SepT, T_IntT, (T_Identifier "x51"), T_EqualT, (T_IntDigit "51"), T_SepT, T_IntT, (T_Identifier "x52"), T_EqualT, (T_IntDigit "52"), T_SepT, T_IntT, (T_Identifier "x53"), T_EqualT, (T_IntDigit "53"), T_SepT, T_IntT, (T_Identifier "x54"), T_EqualT, (T_IntDigit "54"), T_SepT, T_IntT, (T_Identifier "x55"), T_EqualT, (T_IntDigit "55"), T_SepT, T_IntT, (T_Identifier "x56"), T_EqualT, (T_IntDigit "56"), T_SepT, T_IntT, (T_Identifier "x57"), T_EqualT, (T_IntDigit "57"), T_SepT, T_IntT, (T_Identifier "x58"), T_EqualT, (T_IntDigit "58"), T_SepT, T_IntT, (T_Identifier "x59"), T_EqualT, (T_IntDigit "59"), T_SepT, T_IntT, (T_Identifier "x60"), T_EqualT, (T_IntDigit "60"), T_SepT, T_IntT, (T_Identifier "x61"), T_EqualT, (T_IntDigit "61"), T_SepT, T_IntT, (T_Identifier "x62"), T_EqualT, (T_IntDigit "62"), T_SepT, T_IntT, (T_Identifier "x63"), T_EqualT, (T_IntDigit "63"), T_SepT, T_IntT, (T_Identifier "x64"), T_EqualT, (T_IntDigit "64"), T_SepT, T_IntT, (T_Identifier "x65"), T_EqualT, (T_IntDigit "65"), T_SepT, T_IntT, (T_Identifier "x66"), T_EqualT, (T_IntDigit "66"), T_SepT, T_IntT, (T_Identifier "x67"), T_EqualT, (T_IntDigit "67"), T_SepT, T_IntT, (T_Identifier "x68"), T_EqualT, (T_IntDigit "68"), T_SepT, T_IntT, (T_Identifier "x69"), T_EqualT, (T_IntDigit "69"), T_SepT, T_IntT, (T_Identifier "x70"), T_EqualT, (T_IntDigit "70"), T_SepT, T_IntT, (T_Identifier "x71"), T_EqualT, (T_IntDigit "71"), T_SepT, T_IntT, (T_Identifier "x72"), T_EqualT, (T_IntDigit "72"), T_SepT, T_IntT, (T_Identifier "x73"), T_EqualT, (T_IntDigit "73"), T_SepT, T_IntT, (T_Identifier "x74"), T_EqualT, (T_IntDigit "74"), T_SepT, T_IntT, (T_Identifier "x75"), T_EqualT, (T_IntDigit "75"), T_SepT, T_IntT, (T_Identifier "x76"), T_EqualT, (T_IntDigit "76"), T_SepT, T_IntT, (T_Identifier "x77"), T_EqualT, (T_IntDigit "77"), T_SepT, T_IntT, (T_Identifier "x78"), T_EqualT, (T_IntDigit "78"), T_SepT, T_IntT, (T_Identifier "x79"), T_EqualT, (T_IntDigit "79"), T_SepT, T_IntT, (T_Identifier "x80"), T_EqualT, (T_IntDigit "80"), T_SepT, T_IntT, (T_Identifier "x81"), T_EqualT, (T_IntDigit "81"), T_SepT, T_IntT, (T_Identifier "x82"), T_EqualT, (T_IntDigit "82"), T_SepT, T_IntT, (T_Identifier "x83"), T_EqualT, (T_IntDigit "83"), T_SepT, T_IntT, (T_Identifier "x84"), T_EqualT, (T_IntDigit "84"), T_SepT, T_IntT, (T_Identifier "x85"), T_EqualT, (T_IntDigit "85"), T_SepT, T_IntT, (T_Identifier "x86"), T_EqualT, (T_IntDigit "86"), T_SepT, T_IntT, (T_Identifier "x87"), T_EqualT, (T_IntDigit "87"), T_SepT, T_IntT, (T_Identifier "x88"), T_EqualT, (T_IntDigit "88"), T_SepT, T_IntT, (T_Identifier "x89"), T_EqualT, (T_IntDigit "89"), T_SepT, T_IntT, (T_Identifier "x90"), T_EqualT, (T_IntDigit "90"), T_SepT, T_IntT, (T_Identifier "x91"), T_EqualT, (T_IntDigit "91"), T_SepT, T_IntT, (T_Identifier "x92"), T_EqualT, (T_IntDigit "92"), T_SepT, T_IntT, (T_Identifier "x93"), T_EqualT, (T_IntDigit "93"), T_SepT, T_IntT, (T_Identifier "x94"), T_EqualT, (T_IntDigit "94"), T_SepT, T_IntT, (T_Identifier "x95"), T_EqualT, (T_IntDigit "95"), T_SepT, T_IntT, (T_Identifier "x96"), T_EqualT, (T_IntDigit "96"), T_SepT, T_IntT, (T_Identifier "x97"), T_EqualT, (T_IntDigit "97"), T_SepT, T_IntT, (T_Identifier "x98"), T_EqualT, (T_IntDigit "98"), T_SepT, T_IntT, (T_Identifier "x99"), T_EqualT, (T_IntDigit "99"), T_SepT, T_IntT, (T_Identifier "x100"), T_EqualT, (T_IntDigit "100"), T_SepT, T_IntT, (T_Identifier "x101"), T_EqualT, (T_IntDigit "101"), T_SepT, T_IntT, (T_Identifier "x102"), T_EqualT, (T_IntDigit "102"), T_SepT, T_IntT, (T_Identifier "x103"), T_EqualT, (T_IntDigit "103"), T_SepT, T_IntT, (T_Identifier "x104"), T_EqualT, (T_IntDigit "104"), T_SepT, T_IntT, (T_Identifier "x105"), T_EqualT, (T_IntDigit "105"), T_SepT, T_IntT, (T_Identifier "x106"), T_EqualT, (T_IntDigit "106"), T_SepT, T_IntT, (T_Identifier "x107"), T_EqualT, (T_IntDigit "107"), T_SepT, T_IntT, (T_Identifier "x108"), T_EqualT, (T_IntDigit "108"), T_SepT, T_IntT, (T_Identifier "x109"), T_EqualT, (T_IntDigit "109"), T_SepT, T_IntT, (T_Identifier "x110"), T_EqualT, (T_IntDigit "110"), T_SepT, T_IntT, (T_Identifier "x111"), T_EqualT, (T_IntDigit "111"), T_SepT, T_IntT, (T_Identifier "x112"), T_EqualT, (T_IntDigit "112"), T_SepT, T_IntT, (T_Identifier "x113"), T_EqualT, (T_IntDigit "113"), T_SepT, T_IntT, (T_Identifier "x114"), T_EqualT, (T_IntDigit "114"), T_SepT, T_IntT, (T_Identifier "x115"), T_EqualT, (T_IntDigit "115"), T_SepT, T_IntT, (T_Identifier "x116"), T_EqualT, (T_IntDigit "116"), T_SepT, T_IntT, (T_Identifier "x117"), T_EqualT, (T_IntDigit "117"), T_SepT, T_IntT, (T_Identifier "x118"), T_EqualT, (T_IntDigit "118"), T_SepT, T_IntT, (T_Identifier "x119"), T_EqualT, (T_IntDigit "119"), T_SepT, T_IntT, (T_Identifier "x120"), T_EqualT, (T_IntDigit "120"), T_SepT, T_IntT, (T_Identifier "x121"), T_EqualT, (T_IntDigit "121"), T_SepT, T_IntT, (T_Identifier "x122"), T_EqualT, (T_IntDigit "122"), T_SepT, T_IntT, (T_Identifier "x123"), T_EqualT, (T_IntDigit "123"), T_SepT, T_IntT, (T_Identifier "x124"), T_EqualT, (T_IntDigit "124"), T_SepT, T_IntT, (T_Identifier "x125"), T_EqualT, (T_IntDigit "125"), T_SepT, T_IntT, (T_Identifier "x126"), T_EqualT, (T_IntDigit "126"), T_SepT, T_IntT, (T_Identifier "x127"), T_EqualT, (T_IntDigit "127"), T_SepT, T_IntT, (T_Identifier "x128"), T_EqualT, (T_IntDigit "128"), T_SepT, T_IntT, (T_Identifier "x129"), T_EqualT, (T_IntDigit "129"), T_SepT, T_IntT, (T_Identifier "x130"), T_EqualT, (T_IntDigit "130"), T_SepT, T_IntT, (T_Identifier "x131"), T_EqualT, (T_IntDigit "131"), T_SepT, T_IntT, (T_Identifier "x132"), T_EqualT, (T_IntDigit "132"), T_SepT, T_IntT, (T_Identifier "x133"), T_EqualT, (T_IntDigit "133"), T_SepT, T_IntT, (T_Identifier "x134"), T_EqualT, (T_IntDigit "134"), T_SepT, T_IntT, (T_Identifier "x135"), T_EqualT, (T_IntDigit "135"), T_SepT, T_IntT, (T_Identifier "x136"), T_EqualT, (T_IntDigit "136"), T_SepT, T_IntT, (T_Identifier "x137"), T_EqualT, (T_IntDigit "137"), T_SepT, T_IntT, (T_Identifier "x138"), T_EqualT, (T_IntDigit "138"), T_SepT, T_IntT, (T_Identifier "x139"), T_EqualT, (T_IntDigit "139"), T_SepT, T_IntT, (T_Identifier "x140"), T_EqualT, (T_IntDigit "140"), T_SepT, T_IntT, (T_Identifier "x141"), T_EqualT, (T_IntDigit "141"), T_SepT, T_IntT, (T_Identifier "x142"), T_EqualT, (T_IntDigit "142"), T_SepT, T_IntT, (T_Identifier "x143"), T_EqualT, (T_IntDigit "143"), T_SepT, T_IntT, (T_Identifier "x144"), T_EqualT, (T_IntDigit "144"), T_SepT, T_IntT, (T_Identifier "x145"), T_EqualT, (T_IntDigit "145"), T_SepT, T_IntT, (T_Identifier "x146"), T_EqualT, (T_IntDigit "146"), T_SepT, T_IntT, (T_Identifier "x147"), T_EqualT, (T_IntDigit "147"), T_SepT, T_IntT, (T_Identifier "x148"), T_EqualT, (T_IntDigit "148"), T_SepT, T_IntT, (T_Identifier "x149"), T_EqualT, (T_IntDigit "149"), T_SepT, T_IntT, (T_Identifier "x150"), T_EqualT, (T_IntDigit "150"), T_SepT, T_IntT, (T_Identifier "x151"), T_EqualT, (T_IntDigit "151"), T_SepT, T_IntT, (T_Identifier "x152"), T_EqualT, (T_IntDigit "152"), T_SepT, T_IntT, (T_Identifier "x153"), T_EqualT, (T_IntDigit "153"), T_SepT, T_IntT, (T_Identifier "x154"), T_EqualT, (T_IntDigit "154"), T_SepT, T_IntT, (T_Identifier "x155"), T_EqualT, (T_IntDigit "155"), T_SepT, T_IntT, (T_Identifier "x156"), T_EqualT, (T_IntDigit "156"), T_SepT, T_IntT, (T_Identifier "x157"), T_EqualT, (T_IntDigit "157"), T_SepT, T_IntT, (T_Identifier "x158"), T_EqualT, (T_IntDigit "158"), T_SepT, T_IntT, (T_Identifier "x159"), T_EqualT, (T_IntDigit "159"), T_SepT, T_IntT, (T_Identifier "x160"), T_EqualT, (T_IntDigit "160"), T_SepT, T_IntT, (T_Identifier "x161"), T_EqualT, (T_IntDigit "161"), T_SepT, T_IntT, (T_Identifier "x162"), T_EqualT, (T_IntDigit "162"), T_SepT, T_IntT, (T_Identifier "x163"), T_EqualT, (T_IntDigit "163"), T_SepT, T_IntT, (T_Identifier "x164"), T_EqualT, (T_IntDigit "164"), T_SepT, T_IntT, (T_Identifier "x165"), T_EqualT, (T_IntDigit "165"), T_SepT, T_IntT, (T_Identifier "x166"), T_EqualT, (T_IntDigit "166"), T_SepT, T_IntT, (T_Identifier "x167"), T_EqualT, (T_IntDigit "167"), T_SepT, T_IntT, (T_Identifier "x168"), T_EqualT, (T_IntDigit "168"), T_SepT, T_IntT, (T_Identifier "x169"), T_EqualT, (T_IntDigit "169"), T_SepT, T_IntT, (T_Identifier "x170"), T_EqualT, (T_IntDigit "170"), T_SepT, T_IntT, (T_Identifier "x171"), T_EqualT, (T_IntDigit "171"), T_SepT, T_IntT, (T_Identifier "x172"), T_EqualT, (T_IntDigit "172"), T_SepT, T_IntT, (T_Identifier "x173"), T_EqualT, (T_IntDigit "173"), T_SepT, T_IntT, (T_Identifier "x174"), T_EqualT, (T_IntDigit "174"), T_SepT, T_IntT, (T_Identifier "x175"), T_EqualT, (T_IntDigit "175"), T_SepT, T_IntT, (T_Identifier "x176"), T_EqualT, (T_IntDigit "176"), T_SepT, T_IntT, (T_Identifier "x177"), T_EqualT, (T_IntDigit "177"), T_SepT, T_IntT, (T_Identifier "x178"), T_EqualT, (T_IntDigit "178"), T_SepT, T_IntT, (T_Identifier "x179"), T_EqualT, (T_IntDigit "179"), T_SepT, T_IntT, (T_Identifier "x180"), T_EqualT, (T_IntDigit "180"), T_SepT, T_IntT, (T_Identifier "x181"), T_EqualT, (T_IntDigit "181"), T_SepT, T_IntT, (T_Identifier "x182"), T_EqualT, (T_IntDigit "182"), T_SepT, T_IntT, (T_Identifier "x183"), T_EqualT, (T_IntDigit "183"), T_SepT, T_IntT, (T_Identifier "x184"), T_EqualT, (T_IntDigit "184"), T_SepT, T_IntT, (T_Identifier "x185"), T_EqualT, (T_IntDigit "185"), T_SepT, T_IntT, (T_Identifier "x186"), T_EqualT, (T_IntDigit "186"), T_SepT, T_IntT, (T_Identifier "x187"), T_EqualT, (T_IntDigit "187"), T_SepT, T_IntT, (T_Identifier "x188"), T_EqualT, (T_IntDigit "188"), T_SepT, T_IntT, (T_Identifier "x189"), T_EqualT, (T_IntDigit "189"), T_SepT, T_IntT, (T_Identifier "x190"), T_EqualT, (T_IntDigit "190"), T_SepT, T_IntT, (T_Identifier "x191"), T_EqualT, (T_IntDigit "191"), T_SepT, T_IntT, (T_Identifier "x192"), T_EqualT, (T_IntDigit "192"), T_SepT, T_IntT, (T_Identifier "x193"), T_EqualT, (T_IntDigit "193"), T_SepT, T_IntT, (T_Identifier "x194"), T_EqualT, (T_IntDigit "194"), T_SepT, T_IntT, (T_Identifier "x195"), T_EqualT, (T_IntDigit "195"), T_SepT, T_IntT, (T_Identifier "x196"), T_EqualT, (T_IntDigit "196"), T_SepT, T_IntT, (T_Identifier "x197"), T_EqualT, (T_IntDigit "197"), T_SepT, T_IntT, (T_Identifier "x198"), T_EqualT, (T_IntDigit "198"), T_SepT, T_IntT, (T_Identifier "x199"), T_EqualT, (T_IntDigit "199"), T_SepT, T_IntT, (T_Identifier "x200"), T_EqualT, (T_IntDigit "200"), T_SepT, T_IntT, (T_Identifier "x201"), T_EqualT, (T_IntDigit "201"), T_SepT, T_IntT, (T_Identifier "x202"), T_EqualT, (T_IntDigit "202"), T_SepT, T_IntT, (T_Identifier "x203"), T_EqualT, (T_IntDigit "203"), T_SepT, T_IntT, (T_Identifier "x204"), T_EqualT, (T_IntDigit "204"), T_SepT, T_IntT, (T_Identifier "x205"), T_EqualT, (T_IntDigit "205"), T_SepT, T_IntT, (T_Identifier "x206"), T_EqualT, (T_IntDigit "206"), T_SepT, T_IntT, (T_Identifier "x207"), T_EqualT, (T_IntDigit "207"), T_SepT, T_IntT, (T_Identifier "x208"), T_EqualT, (T_IntDigit "208"), T_SepT, T_IntT, (T_Identifier "x209"), T_EqualT, (T_IntDigit "209"), T_SepT, T_IntT, (T_Identifier "x210"), T_EqualT, (T_IntDigit "210"), T_SepT, T_IntT, (T_Identifier "x211"), T_EqualT, (T_IntDigit "211"), T_SepT, T_IntT, (T_Identifier "x212"), T_EqualT, (T_IntDigit "212"), T_SepT, T_IntT, (T_Identifier "x213"), T_EqualT, (T_IntDigit "213"), T_SepT, T_IntT, (T_Identifier "x214"), T_EqualT, (T_IntDigit "214"), T_SepT, T_IntT, (T_Identifier "x215"), T_EqualT, (T_IntDigit "215"), T_SepT, T_IntT, (T_Identifier "x216"), T_EqualT, (T_IntDigit "216"), T_SepT, T_IntT, (T_Identifier "x217"), T_EqualT, (T_IntDigit "217"), T_SepT, T_IntT, (T_Identifier "x218"), T_EqualT, (T_IntDigit "218"), T_SepT, T_IntT, (T_Identifier "x219"), T_EqualT, (T_IntDigit "219"), T_SepT, T_IntT, (T_Identifier "x220"), T_EqualT, (T_IntDigit "220"), T_SepT, T_IntT, (T_Identifier "x221"), T_EqualT, (T_IntDigit "221"), T_SepT, T_IntT, (T_Identifier "x222"), T_EqualT, (T_IntDigit "222"), T_SepT, T_IntT, (T_Identifier "x223"), T_EqualT, (T_IntDigit "223"), T_SepT, T_IntT, (T_Identifier "x224"), T_EqualT, (T_IntDigit "224"), T_SepT, T_IntT, (T_Identifier "x225"), T_EqualT, (T_IntDigit "225"), T_SepT, T_IntT, (T_Identifier "x226"), T_EqualT, (T_IntDigit "226"), T_SepT, T_IntT, (T_Identifier "x227"), T_EqualT, (T_IntDigit "227"), T_SepT, T_IntT, (T_Identifier "x228"), T_EqualT, (T_IntDigit "228"), T_SepT, T_IntT, (T_Identifier "x229"), T_EqualT, (T_IntDigit "229"), T_SepT, T_IntT, (T_Identifier "x230"), T_EqualT, (T_IntDigit "230"), T_SepT, T_IntT, (T_Identifier "x231"), T_EqualT, (T_IntDigit "231"), T_SepT, T_IntT, (T_Identifier "x232"), T_EqualT, (T_IntDigit "232"), T_SepT, T_IntT, (T_Identifier "x233"), T_EqualT, (T_IntDigit "233"), T_SepT, T_IntT, (T_Identifier "x234"), T_EqualT, (T_IntDigit "234"), T_SepT, T_IntT, (T_Identifier "x235"), T_EqualT, (T_IntDigit "235"), T_SepT, T_IntT, (T_Identifier "x236"), T_EqualT, (T_IntDigit "236"), T_SepT, T_IntT, (T_Identifier "x237"), T_EqualT, (T_IntDigit "237"), T_SepT, T_IntT, (T_Identifier "x238"), T_EqualT, (T_IntDigit "238"), T_SepT, T_IntT, (T_Identifier "x239"), T_EqualT, (T_IntDigit "239"), T_SepT, T_IntT, (T_Identifier "x240"), T_EqualT, (T_IntDigit "240"), T_SepT, T_IntT, (T_Identifier "x241"), T_EqualT, (T_IntDigit "241"), T_SepT, T_IntT, (T_Identifier "x242"), T_EqualT, (T_IntDigit "242"), T_SepT, T_IntT, (T_Identifier "x243"), T_EqualT, (T_IntDigit "243"), T_SepT, T_IntT, (T_Identifier "x244"), T_EqualT, (T_IntDigit "244"), T_SepT, T_IntT, (T_Identifier "x245"), T_EqualT, (T_IntDigit "245"), T_SepT, T_IntT, (T_Identifier "x246"), T_EqualT, (T_IntDigit "246"), T_SepT, T_IntT, (T_Identifier "x247"), T_EqualT, (T_IntDigit "247"), T_SepT, T_IntT, (T_Identifier "x248"), T_EqualT, (T_IntDigit "248"), T_SepT, T_IntT, (T_Identifier "x249"), T_EqualT, (T_IntDigit "249"), T_SepT, T_IntT, (T_Identifier "x250"), T_EqualT, (T_IntDigit "250"), T_SepT, T_IntT, (T_Identifier "x251"), T_EqualT, (T_IntDigit "251"), T_SepT, T_IntT, (T_Identifier "x252"), T_EqualT, (T_IntDigit "252"), T_SepT, T_IntT, (T_Identifier "x253"), T_EqualT, (T_IntDigit "253"), T_SepT, T_IntT, (T_Identifier "x254"), T_EqualT, (T_IntDigit "254"), T_SepT, T_IntT, (T_Identifier "x255"), T_EqualT, (T_IntDigit "255"), T_SepT, T_IntT, (T_Identifier "x256"), T_EqualT, (T_IntDigit "256"), T_EndT]

negIntDeclaration :: Assertion
negIntDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/negIntDeclaration.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, T_MinusToken, (T_IntDigit "1"), T_EndT]

puncCharDeclaration :: Assertion
puncCharDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/puncCharDeclaration.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'!'"), T_EndT]

stringDeclaration :: Assertion
stringDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/stringDeclaration.wacc" ))
  @=? [ T_BeginT, T_StringT, (T_Identifier "s"), T_EqualT, (T_StringLiteral "\"Hello World!\""), T_EndT]

zeroIntDeclaration :: Assertion
zeroIntDeclaration = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/variables/zeroIntDeclaration.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_EndT]

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
fibonacciFullIt = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/fibonacciFullIt.wacc" ))
  @=? [ T_BeginT, T_PrintLnT, (T_StringLiteral "\"This program calculates the nth fibonacci number iteratively.\""), T_SepT, T_PrintT, (T_StringLiteral "\"Please enter n (should not be too large): \""), T_SepT, T_IntT, (T_Identifier "n"), T_EqualT, (T_IntDigit "0"), T_SepT, T_ReadT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\"The input n is \""), T_SepT, T_PrintLnT, (T_Identifier "n"), T_SepT, T_PrintT, (T_StringLiteral "\"The nth fibonacci number is \""), T_SepT, T_IntT, (T_Identifier "f0"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "f1"), T_EqualT, (T_IntDigit "1"), T_SepT, T_IntT, (T_Identifier "save"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "n"), T_GreaterT, (T_IntDigit "0"), T_DoT, (T_Identifier "save"), T_EqualT, (T_Identifier "f0"), T_SepT, (T_Identifier "f0"), T_EqualT, (T_Identifier "f1"), T_SepT, (T_Identifier "f1"), T_EqualT, (T_Identifier "save"), T_PlusToken, (T_Identifier "f1"), T_SepT, (T_Identifier "n"), T_EqualT, (T_Identifier "n"), T_MinusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_Identifier "f0"), T_EndT]

fibonacciIterative :: Assertion
fibonacciIterative = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/fibonacciIterative.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "f0"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "f1"), T_EqualT, (T_IntDigit "1"), T_SepT, T_IntT, (T_Identifier "save"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintLnT, (T_StringLiteral "\"The first 20 fibonacci numbers are:\""), T_SepT, T_WhileT, (T_Identifier "i"), T_LessT, (T_IntDigit "20"), T_DoT, T_PrintT, (T_Identifier "f0"), T_SepT, T_PrintT, (T_StringLiteral "\", \""), T_SepT, (T_Identifier "save"), T_EqualT, (T_Identifier "f0"), T_SepT, (T_Identifier "f0"), T_EqualT, (T_Identifier "f1"), T_SepT, (T_Identifier "f1"), T_EqualT, (T_Identifier "save"), T_PlusToken, (T_Identifier "f1"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"...\""), T_EndT]

loopCharCondition :: Assertion
loopCharCondition = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/loopCharCondition.wacc" ))
  @=? [ T_BeginT, T_CharT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'\\0'"), T_SepT, T_WhileT, (T_Identifier "c"), T_EqT, (T_CharLiteral "'\\0'"), T_DoT, (T_Identifier "c"), T_EqualT, (T_CharLiteral "'a'"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Change c\""), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"Should print \\\"Change c\\\" once before.\""), T_EndT]

loopIntCondition :: Assertion
loopIntCondition = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/loopIntCondition.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "n"), T_EqualT, (T_IntDigit "0"), T_SepT, T_WhileT, (T_Identifier "n"), T_NotEqT, (T_IntDigit "1"), T_DoT, (T_Identifier "n"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Change n\""), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"Should print \\\"Change n\\\" once before.\""), T_EndT]

maxT :: Assertion
maxT = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/max.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "17"), T_SepT, T_WhileT, T_LParenT, (T_Identifier "y"), T_GreaterT, (T_IntDigit "0"), T_OrT, (T_Identifier "x"), T_GreaterT, (T_IntDigit "0"), T_RParenT, T_DoT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_MinusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "y"), T_EqualT, (T_Identifier "y"), T_MinusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintT, (T_StringLiteral "\"max value = \""), T_SepT, T_PrintLnT, (T_Identifier "i"), T_EndT]

minT :: Assertion
minT = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/min.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "10"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "17"), T_SepT, T_WhileT, (T_Identifier "y"), T_GreaterT, (T_IntDigit "0"), T_AndT, (T_Identifier "x"), T_GreaterT, (T_IntDigit "0"), T_DoT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_MinusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "y"), T_EqualT, (T_Identifier "y"), T_MinusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintT, (T_StringLiteral "\"min value = \""), T_SepT, T_PrintLnT, (T_Identifier "i"), T_EndT]

rmStyleAdd :: Assertion
rmStyleAdd = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/rmStyleAdd.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "3"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "7"), T_SepT, T_PrintT, (T_StringLiteral "\"initial value of x: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_WhileT, (T_Identifier "y"), T_GreaterT, (T_IntDigit "0"), T_DoT, T_PrintT, (T_StringLiteral "\"(+)\""), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "y"), T_EqualT, (T_Identifier "y"), T_MinusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_PrintT, (T_StringLiteral "\"final value of x: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

rmStyleAddIO :: Assertion
rmStyleAddIO = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/rmStyleAddIO.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "x"), T_EqualT, (T_IntDigit "0"), T_SepT, T_IntT, (T_Identifier "y"), T_EqualT, (T_IntDigit "0"), T_SepT, T_PrintT, (T_StringLiteral "\"Enter the first number: \""), T_SepT, T_ReadT, (T_Identifier "x"), T_SepT, T_PrintT, (T_StringLiteral "\"Enter the second number: \""), T_SepT, T_ReadT, (T_Identifier "y"), T_SepT, T_PrintT, (T_StringLiteral "\"Initial value of x: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_SepT, T_WhileT, (T_Identifier "y"), T_GreaterT, (T_IntDigit "0"), T_DoT, T_PrintT, (T_StringLiteral "\"(+)\""), T_SepT, (T_Identifier "x"), T_EqualT, (T_Identifier "x"), T_PlusToken, (T_IntDigit "1"), T_SepT, (T_Identifier "y"), T_EqualT, (T_Identifier "y"), T_MinusToken, (T_IntDigit "1"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"\""), T_SepT, T_PrintT, (T_StringLiteral "\"final value of x: \""), T_SepT, T_PrintLnT, (T_Identifier "x"), T_EndT]

whileBasic :: Assertion
whileBasic = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/whileBasic.wacc" ))
  @=? [ T_BeginT, T_WhileT, T_FalseToken, T_DoT, T_SkipT, T_DoneT, T_EndT]

whileBoolFlip :: Assertion
whileBoolFlip = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/whileBoolFlip.wacc" ))
  @=? [ T_BeginT, T_BoolT, (T_Identifier "b"), T_EqualT, T_TrueToken, T_SepT, T_WhileT, (T_Identifier "b"), T_DoT, T_PrintLnT, (T_StringLiteral "\"flip b!\""), T_SepT, (T_Identifier "b"), T_EqualT, T_NotT, (T_Identifier "b"), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"end of loop\""), T_EndT]

whileCount :: Assertion
whileCount = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/whileCount.wacc" ))
  @=? [ T_BeginT, T_IntT, (T_Identifier "i"), T_EqualT, (T_IntDigit "1"), T_SepT, T_PrintLnT, (T_StringLiteral "\"Can you count to 10?\""), T_SepT, T_WhileT, (T_Identifier "i"), T_LessEqT, (T_IntDigit "10"), T_DoT, T_PrintLnT, (T_Identifier "i"), T_SepT, (T_Identifier "i"), T_EqualT, (T_Identifier "i"), T_PlusToken, (T_IntDigit "1"), T_DoneT, T_EndT]

whileFalse :: Assertion
whileFalse = strip (tokens (
  unsafePerformIO $ readFile "src-test/wacc-samples/valid/while/whileFalse.wacc" ))
  @=? [ T_BeginT, T_WhileT, T_FalseToken, T_DoT, T_PrintLnT, (T_StringLiteral "\"looping...\""), T_DoneT, T_SepT, T_PrintLnT, (T_StringLiteral "\"end of loop\""), T_EndT]

syntacticTests :: [TestTree]
syntacticTests = undefined

semanticTests :: [TestTree]
semanticTests = undefined
