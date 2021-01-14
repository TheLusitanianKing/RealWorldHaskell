-- the following signature is equivalent to
-- returnSingleton :: a -> [a]
returnSingleton :: a -> [] a
returnSingleton x = [x]

-- duplicate definition for illustration purpose
instance Monad [] where
    return x = [x]
    xs >>= f = concatMap f xs