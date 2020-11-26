data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

treeLengths :: Tree String -> Tree Int
treeLengths (Leaf value)      = Leaf (length value)
treeLengths (Node left right) = Node (treeLengths left) (treeLengths right)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)          = Leaf (f x)
treeMap f (Node left right) = Node (treeMap f left) (treeMap f right)

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
    fmap = treeMap

-- instance Functor [] where
--     fmap = map

data Foo a = Foo a

instance Functor Foo where
    fmap f (Foo a) = Foo (f a)