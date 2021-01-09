module Calculator.Test
  ( -- * Functions
    testCalculate
  )
where

import Lab3.Calculator.Parser (calculate)
import Test.Hspec (Expectation, SpecWith, describe, it, shouldBe)

-- | Unit tests for an automatically generated calculator
testCalculate :: SpecWith ()
testCalculate = describe "Calculator - calculate" $ do
  uTestSimple
  uTestBracket
  uTestOp

-- | Unit tests for calculate.
-- Tests for simple operations.
uTestSimple :: SpecWith ()
uTestSimple = it "calculate - simple expressions" $ do
  calc "2 + 2 * 2" 6
  calc "123 + 987 * 654" 645621
  calc "1000 + 1 + 10 + 100" 1111
  calc "2+2" 4
  calc "2 + 2 * 2 / 2" 4
  calc "1 * 1 * 1 / 1 + 1 + 2 * 2 / 2" 4

-- | Unit tests for calculate.
-- Testing the correct processing of brackets.
uTestBracket :: SpecWith ()
uTestBracket = it "calculate - brackets expressions" $ do
  calc "(2 + 2)\t\n * 2 \n + 2" 10
  calc "(2 + 2) * (2 + 2)" 16
  calc "2+2*(2/2+2)" 8
  calc "1*2*(1/1+1)+2*2/2" 6

-- | Unit tests for calculate.
-- Testing the correct associativity and the new 
-- operation (exponentiation - right-associative).
uTestOp :: SpecWith ()
uTestOp = it "calculate - operations test" $ do
  calc "3 - 2- 1" 0
  calc "8/4/2" 1
  calc "8/4*2" 4
  calc "2 ** 3 ** 2" 512
  
calc :: String -> Double -> Expectation
calc expr expected = calculate expr `shouldBe` Just expected
