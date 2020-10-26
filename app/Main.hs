module Main where

import Control.Monad.Trans.State.Lazy
import Token
import Tree

main :: IO ()
main = print (evaluateTree (lexicalAnalyzer "aandb"))
