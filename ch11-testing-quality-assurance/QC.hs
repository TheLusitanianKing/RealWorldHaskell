module QC where

import Test.QuickCheck
import Prettify2
import Data.List ((\\), sort)
import Control.Monad (liftM, liftM2)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

-- as head and minimum do not work on empty lists
-- using the (==>) implication function, we filter out invalid data before running the property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)
    -- (\\) is the list difference function

prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

-- testing against a model
prop_sort_model xs = sort xs == qsort xs

-- instance Arbitrary Char where
--     arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

-- instance Arbitrary Doc where
--     arbitrary = do
--         n <- choose (1,6) :: Gen Int
--         case n of
--             1 -> return Empty
--             2 -> do x <- arbitrary
--                     return (Char x)
--             3 -> do x <- arbitrary
--                     return (Text x)
--             4 -> return Line
--             5 -> do x <- arbitrary
--                     y <- arbitrary
--                     return (Concat x y)
--             6 -> do x <- arbitrary
--                     y <- arbitrary
--                     return (Union x y)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty,
                liftM Char arbitrary,
                liftM Text arbitrary,
                return Line,
                liftM2 Concat arbitrary arbitrary,
                liftM2 Union arbitrary arbitrary ]