data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)

-- Here's a brief recap of Haskell's three ways to introduce new names for types:
-- The data keyword introduces a truly new albegraic data type.
-- The type keyword gives us a synonym to use for an existing type.
--      We can use the type and its synonym interchangeably.
-- The newtype keyword gives an existing type a distinct identity.
--      The original type and the new type are not interchangeable.