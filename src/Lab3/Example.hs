module Lab3.Example
  ( -- * Functions
    testFromFile
  )
where

import Data.Maybe (fromJust)
import Lab3.Calculator.Parser (calculate)
import qualified Lab3.PythonToC.Lexer as Interpreter (lexer)
import Lab3.PythonToC.Parser (interpret)
import System.IO (IOMode (..), hClose, hGetContents, openFile)

-- | Run the generated parser on the data from the input files.
-- input files: test_py.in and test_calc.in
-- output files: test_py.out and test_calc.out
testFromFile :: IO ()
testFromFile =
  do
    runFromFile "test_py.in" "test_py.lex" (fmap show . Just . Interpreter.lexer)
    runFromFile "test_py.in" "test_py.check" Just
    runFromFile "test_calc.in" "test_calc.out" (fmap show . calculate)
    runFromFile "test_py.in" "test_py.out" interpret

runFromFile :: String -> String -> (String -> Maybe String) -> IO ()
runFromFile fromFile toFile action =
  do
    handle <- openFile fromFile ReadMode
    everything <- hGetContents handle
    writeFile toFile (fromJust . action $ everything)
    hClose handle
