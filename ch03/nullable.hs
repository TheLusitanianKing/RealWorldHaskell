someString = Just "something"
someBool = Just True

-- Maybe is an example of a parameterised type
-- data Maybe a = Just a | Nothing
-- a is a type variable into a type declaration

wrapped = Just (Just "wrapped")