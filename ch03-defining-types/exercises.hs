import qualified Data.List

-- Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard length function.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Write a function that computes the mean of a list
-- i.e. the sum of all elements in the list divided by its length.
-- (You may need to use the fromIntegral function to convert the length of the list
-- from an integer into a floating point number.) 
myMean :: [Int] -> Double
myMean l = fromIntegral (sum l) / fromIntegral (length l)

-- Turn a list into a palindrome,
-- i.e. it should read the same both backwards and forwards.
-- For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
turnToPalindrome :: [a] -> [a]
turnToPalindrome l | length l <= 1  = l
turnToPalindrome l                  = doTurnToPalindrome l l []
    where doTurnToPalindrome original [] acc     = original ++ acc
          doTurnToPalindrome original (x:xs) acc = doTurnToPalindrome original xs (x:acc)

-- Write a function that determines whether its input list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x | length x <= 1      = True
isPalindrome (x:xs) | x == last xs  = isPalindrome $ init xs
isPalindrome _                      = False

-- Create a function that sorts a list of lists based on the length of each sublist.
-- (You may want to look at the sortBy function from the Data.List module.)
sortBySubListLength :: [[a]] -> [[a]]
sortBySubListLength = Data.List.sortBy comparisonFunction
    where comparisonFunction a b = compare (length a) (length b)

sortBySubListLengthAlt :: [[a]] -> [[a]]
sortBySubListLengthAlt = Data.List.sortBy (\a b -> compare (length a) (length b))

-- Define a function that joins a list of lists together using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse _ []       = []
intersperse _ [x]      = x
intersperse sep (x:xs) = x ++ (sep : (intersperse sep xs))

-- Using the binary tree type that we defined earlier in this chapter,
-- write a function that will determine the height of the tree.
-- The height is the largest number of hops from the root to an Empty.
-- For example, the tree Empty has height zero;
-- Node "x" Empty Empty has height one;
-- Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a) | Empty
              deriving (Show)
treeLength :: Tree a -> Int
treeLength Empty          = 0
treeLength (Node _ t1 t2) = 1 + treeLength t1 + treeLength t2

-- Consider three two-dimensional points a, b, and c.
-- If we look at the angle formed by the line segment from a to b and the line segment from b to c,
-- it either turns left, turns right, or forms a straight line.
-- Define a Direction data type that lets you represent these possibilities.
data Coordinate = Coordinate Double Double
data Direction = Left | Right | Straight

-- Write a function that calculates the turn made by three 2D points and returns a Direction.
-- direction :: Coordinate -> Coordinate -> Coordinate -> Direction