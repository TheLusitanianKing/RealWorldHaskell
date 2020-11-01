add :: Int -> Int -> Int 
add a b = a + b

myNot :: Bool -> Bool
myNot True  = False
myNot False = True

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
