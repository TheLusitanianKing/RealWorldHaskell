myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step acc (x:xs) = myFoldl step (step acc x) xs
myFoldl _    acc _      = acc

-- foldl (+) 0 (1:2:3:[])
--           == foldl (+) (0 + 1)             (2:3:[])
--           == foldl (+) ((0 + 1) + 2)       (3:[])
--           == foldl (+) (((0 + 1) + 2) + 3) []
--           ==           (((0 + 1) + 2) + 3)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr step acc (x:xs) = step x (myFoldr step acc xs)
myFoldr _    acc _      = acc

-- foldr (+) 0 (1:2:3:[])
--           == 1 +           foldr (+) 0 (2:3:[])
--           == 1 + (2 +      foldr (+) 0 (3:[])
--           == 1 + (2 + (3 + foldr (+) 0 []))
--           == 1 + (2 + (3 + 0))

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []                 = []
myFilter p (x:xs) | p x       = x : myFilter p xs
                  | otherwise = myFilter p xs
-- using foldr
myFilterAlt :: (a -> Bool) -> [a] -> [a]
myFilterAlt p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
-- The class of functions that we can express using foldr is called primitive recursive

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr step [] xs
    where step x ys = f x : ys

myMapAlt :: (a -> b) -> [a] -> [b]
myMapAlt f = foldr (\x ys -> f x : ys) []

myFoldlWithFoldr :: (a -> b -> a) -> a -> [b] -> a
myFoldlWithFoldr f z xs = foldr step id xs z
    where step x g a = g (f a x) -- TODO: understand foldl with foldr

identity :: [a] -> [a]
identity = foldr (:) []

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) xs ys