module Calculator.Test
  ( -- * Functions
    testCalculate
  )
where

import Lab3.Calculator.Parser (calculate)
import Test.Hspec (Expectation, SpecWith, describe, it, shouldBe)

-- | Unit tests for the moving average function with a description
testCalculate :: SpecWith ()
testCalculate = describe "Calculator - calculate" $ do
  uTestSimple
  uTestBracket

calc :: String -> Integer -> Expectation
calc expr expected = calculate expr `shouldBe` Just expected

-- | Unit tests for calculate
uTestSimple :: SpecWith ()
uTestSimple = it "calculate - simple expressions" $ do
  calc "2 + 2 * 2" 6
  calc "123 + 987 * 654" 645621
  calc "1000 + 1 + 10 + 100" 1111
  calc "2+2" 4
  calc "2 + 2 * 2 / 2" 4
  calc "1 * 1 * 1 / 1 + 1 + 2 * 2 / 2" 4

-- | Unit tests for calculate
uTestBracket :: SpecWith ()
uTestBracket = it "calculate - brackets expressions" $ do
  calc "(2 + 2)\t\n * 2 \n + 2" 10
  calc "(2 + 2) * (2 + 2)" 16
  calc "2+2*(2/2+2)" 8
  calc "1*2*(1/1+1)+2*2/2" 6
