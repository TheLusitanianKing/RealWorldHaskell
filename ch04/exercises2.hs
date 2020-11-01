import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

-- tail-recursive function (because the last thing that loop does is simply call itself)
loop acc []     = acc -- base case (or terminating case)
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs -- recursive case (or inductive case)

-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the section called “Explicit recursion”. 
asIntFold :: String -> Int
asIntFold []       = error "Empty list"
asIntFold ('-':xs) = (-1) * asIntFold xs
asIntFold xs       = foldl (\a b -> a * 10 + digitToInt b) 0 xs

-- The Prelude function concat concatenates a list of lists into a single list
-- Write your own definition of concat using foldr
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- Write your own definition of the standard takeWhile function
-- First using explicit recursion, then foldr.
takeWhileRecursion :: (a -> Bool) -> [a] -> [a]
takeWhileRecursion f (x:xs) | f x = x : takeWhileRecursion f xs
takeWhileRecursion _ _            = []

takeWhileFoldR :: (a -> Bool) -> [a] -> [a]
takeWhileFoldR f = foldr step []
    where step a b | f a = a : b
          step _ _ = []

-- The Data.List module defines a function, groupBy, which has the following type
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- Use ghci to load the Data.List module and figure out what groupBy does
-- Then write your own implementation using a fold.
-- myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- any with foldr
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

myCycle :: [a] -> [a]
myCycle l = foldr (:) (myCycle l) l

-- myWords :: String -> [String] TODO 

myUnlines :: [String] -> String
myUnlines = foldr (\x y -> x ++ "\n" ++ y) []