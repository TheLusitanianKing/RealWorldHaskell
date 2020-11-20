data List a = Cons a (List a) | Nil
              deriving (Show)

list1 = Cons 1 (Cons 2 (Cons 3 Nil))

fromList :: [a] -> List a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil      = []
toList (Cons x l) = x:(toList l)