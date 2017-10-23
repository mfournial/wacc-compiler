import System.Exit
import System.Environment

import qualified Data.ByteString as B

--import Lib (inc)

type UserInput = String

type CompileOutput = Either CompileError MachineCode

type CompileError = (String, ExitCode)
type MachineCode = B.ByteString

validateInput :: [String] -> Either String UserInput
validateInput = Right . head

handleBadInput :: String -> IO ()
handleBadInput = undefined

handleError :: CompileError -> IO ()
handleError (s, c) = print s >> exitWith c

writeOutput :: MachineCode -> IO ()
writeOutput = undefined

compileInput :: UserInput -> CompileOutput
compileInput = undefined

compile :: UserInput -> IO ()
compile = either handleError writeOutput . compileInput

main :: IO ()
main = do
  input <- fmap validateInput getArgs
  either handleBadInput compile input
