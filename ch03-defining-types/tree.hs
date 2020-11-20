data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)

data TreeAlt a = NodeAlt a (Maybe (TreeAlt a)) (Maybe (TreeAlt a)) deriving (Show)
simpleTreeAlt = NodeAlt "parent" (Just (NodeAlt "left child" Nothing Nothing)) (Just (NodeAlt "right child" Nothing Nothing))