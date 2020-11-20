data Color = Red | Green | Blue
     deriving (Read, Show, Eq, Ord)
-- For many simple data types, the Haskell compiler can automatically derive instances
-- of Read, Show, Bounded, Enum, Eq, and Ord for us