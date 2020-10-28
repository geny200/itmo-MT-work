module TestTree
  ( -- * Functions
    baseTests
  )
where

import Common
import Test.HUnit
import Token
import Tree
import Data.Either (fromRight)

testsSimpleAndOrXor :: Test
testsSimpleAndOrXor =
  TestList
    [ 
      myTest "a and b"
        (Right (Just (Node [Leaf Var,Leaf And,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a and b"))),

      myTest "a or b"
        (Right (Just (Node [Leaf Var,Leaf Or,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a or b"))),

      myTest "a xor b"
        (Right (Just (Node [Leaf Var,Leaf Xor,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a xor b")))
    ]

testsSimpleNot :: Test
testsSimpleNot =
  TestList
    [
      myTest "not b"
        (Right (Just (Node [Leaf Not, Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "not b"))),

      myTest "not not b"
        (Right (Just (Node [Leaf Not,Node [Leaf Not,Leaf Var]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "not not b"))),

      myTest "a xor not b"
        (Right (Just (Node [Leaf Var,Leaf Xor,
        Node [Leaf Not,Leaf Var]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a xor not b"))),

      myTest "not a and not b"
        (Right (Just (Node [Node [Leaf Not,Leaf Var],Leaf Xor,
        Node [Leaf Not,Leaf Var]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "not a xor not b")))
    ]

testsLevel :: Test
testsLevel =
  TestList
    [
      myTest "a or b and c or d"
         (Right (Just (Node [Node [Leaf Var,Leaf Or,Node [Leaf Var,
         Leaf And,Leaf Var]],Leaf Or,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a or b and c or d"))),

      myTest "a xor b and c xor d"
         (Right (Just (Node [Node [Leaf Var,Leaf Xor,Node [Leaf Var,
         Leaf And,Leaf Var]],Leaf Xor,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a xor b and c xor d"))),

      myTest "a and b xor c and d"
        (Right (Just (Node [Node [Leaf Var,Leaf And,Leaf Var],Leaf Xor,Node
        [Leaf Var,Leaf And,Leaf Var]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a and b xor c and d")))
    ]


testsBrackets :: Test
testsBrackets =
  TestList
    [
      myTest "(a or b) and (c or d)"
        (Right (Just (Node [Node [Leaf BrOpen,Node [Leaf Var,Leaf Or,Leaf Var],
        Leaf BrClose],Leaf And,Node [Leaf BrOpen,Node [Leaf Var,Leaf Or,Leaf Var],
        Leaf BrClose]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "(a or b) and (c or d)"))),

      myTest "(a xor b) and (c xor d)"
        (Right (Just (Node [Node [Leaf BrOpen,Node [Leaf Var,Leaf Xor,Leaf Var],
        Leaf BrClose],Leaf And,Node [Leaf BrOpen,Node [Leaf Var,Leaf Xor,Leaf Var],
        Leaf BrClose]])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "(a xor b) and (c xor d)"))),

      myTest "a and (b xor c) and d"
        ( Right (Just (Node [Node [Leaf Var,Leaf And,Node [Leaf BrOpen,
        Node [Leaf Var,Leaf Xor,Leaf Var],Leaf BrClose]],
        Leaf And,Leaf Var])))
        (evaluateTree (fromRight [] (lexicalAnalyzer "a and (b xor c) and d")))
    ]

baseTests :: Test
baseTests
  = TestList [
      TestLabel "test-simple-or-xor-and" testsSimpleAndOrXor,
      TestLabel "test-simple-not"        testsSimpleNot,
      TestLabel "test-level-operation"   testsLevel,
      TestLabel "test-brackets"          testsBrackets
    ]
