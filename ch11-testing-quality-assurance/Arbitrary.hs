module Arbitrary where

import Test.QuickCheck

-- QuickCheck gives us:
-- class Arbitrary a where
--     arbitrary :: Gen a
--     elements :: [a] -> Gen a
--       (elements takes a list of values, and returns a generator of random values from that list)
--     choose :: Random a => (a, a) -> Gen a
--     oneof :: [Gen a] -> Gen a

data Ternary = Yes | No | Unknown
             deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

-- another possibility :
-- instance Arbitrary Ternary where
--   arbitrary     = do
--       n <- choose (0, 2) :: Gen Int
--       return $ case n of
--                     0 -> Yes
--                     1 -> No
--                     _ -> Unknown

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
--     arbitrary = do
--         x <- arbitrary
--         y <- arbitrary
--         return (x, y)