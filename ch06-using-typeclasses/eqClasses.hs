class BasicEq a where -- declaring a typeclass
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True True   = True
    isEqual False False = True
    isEqual _ _         = False

class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool

-- Rather than making users of the typeclass define both functions for all types,
-- we can provide default implementations for them.
-- Then, users will only have to implement one function.
class BasicEq3 a where
    -- NB: when multiple functions have the same signature, you specify them together
    isEqual3, isNotEqual3 :: a -> a -> Bool
    isEqual3 a b = not $ isNotEqual3 a b
    isNotEqual3 a b = not $ isEqual3 a b

data Color = Red | Green | Blue

instance BasicEq3 Color where
    isEqual3 Red Red     = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue   = True
    isEqual3 _ _         = False

-- NB: Show is usually used to define a String representation
-- for data that is useful for a machine to parse back with Read
instance Show Color where
    show Red   = "Vermelho"
    show Green = "Verde"
    show Blue  = "Azul"

instance Read Color where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs. Each pair has a string and the desired return value.
        -- tryParse will try to match the input to one of these strings.
        tryParse [("Vermelho", Red), ("Verde", Green), ("Azul", Blue)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                  -- Compare the start of the string to be parsed to the text we are looking for.
                  if (take (length attempt) value) == attempt
                      -- If we have a match, return the result and the remaining input
                      then [(result, drop (length attempt) value)]
                      -- If we don't have a match, try the next pair in the list of attempts.
                      else tryParse xs