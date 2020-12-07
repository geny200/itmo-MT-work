import Calculator.Test (testCalculate)
import Test.HUnit
import Test.Hspec (hspec)
import TestTree

-- | Runs Unit tests and Property-based from the packages
main :: IO ()
main =
  hspec $ do
    testCalculate

--  do
--    runTestTT baseTests
