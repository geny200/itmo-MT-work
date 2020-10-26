module Common
  ( -- * Functions
    myTest
  ) where
import Test.HUnit

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
myTest
  :: (Eq a, Show a)
  => String     -- ^ message
  -> a          -- ^ expected value
  -> a          -- ^ actual value
  -> Test       -- ^ resulting `Test`
myTest name x y
  = TestCase (assertEqual (foldr1 (++)
    ["for (", name, "),"])
    x y)