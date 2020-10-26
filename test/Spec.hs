import Test.HUnit
import TestTree

-- | Runs Unit tests and Property-based from the packages
main :: IO Counts
main =
  do
    runTestTT baseTests
