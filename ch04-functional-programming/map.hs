import Data.Char (toUpper)

square :: [Double] -> [Double]
square []     = []
square (x:xs) = x*x : square xs

upperCase :: String -> String
upperCase []     = []
upperCase (x:xs) = toUpper x : upperCase xs

-- using map
square2 :: [Double] -> [Double]
square2 x = map squareOne x
    where squareOne x = x * x

upperCase2 :: String -> String
upperCase2 x = map toUpper x

-- using my knowledge so far
square3 :: [Double] -> [Double]
square3 = map (\x -> x * x)

-- map is a higher-order function (takes other functions as arguments or returns one)
myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _     = [] -- the list type has only 2 constructors (the non-empty list and the empty list)