import Data.List
import Data.Char

suffixes :: [a] -> [[a]]
suffixes l@(x:xs) = l : suffixes xs -- the @ is called an as-pattern
-- it binds the variable l to the value that matches the right side of the @ symbol
suffixes _        = []

suffixes2 :: [a] -> [[a]]
suffixes2 l = init (tails l)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes3 :: [a] -> [[a]]
suffixes3 = compose init tails

suffixes4 :: [a] -> [[a]]
suffixes4 = init . tails -- composition operator

capCount :: String -> Int
capCount = length . filter (isUpper . head) . words