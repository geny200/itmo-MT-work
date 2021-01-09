import Calculator.Test (testCalculate)
import Lab2.TestTree (baseTests)
import Test.HUnit (runTestTT)
import Test.Hspec (hspec)

-- | Running Unit tests for 2 lab - manual parser,
-- and for 3 lab - calculator, together with 4 lab,
-- because I generated it with my code (Sheol generator).
main :: IO ()
main =
  do
    hspec $ do
      testCalculate
    res <- runTestTT baseTests
    print res
