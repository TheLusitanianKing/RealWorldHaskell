-- chain :: m a -> (a -> m b) -> m b
-- inject :: a -> m a

-- It is exactly these three properties, and a few rules about how we can use them together,
-- that define a monad in Haskell.

-- Let's revisit the above list in condensed form.
--       A type constructor m.
--       A function of type m a -> (a -> m b) -> m b for chaining the output of one function into the input of another.
--       A function of type a -> m a for injecting a normal value into the chain, i.e. it wraps a type a with the type constructor m.

-- We have intentionally said nothing about how the chaining and injection functions of a monad should behave,
-- and that's because this almost doesn't matter.
-- In fact, monads are ubiquitous in Haskell code precisely because they are so simple.
-- Many common programming patterns have a monadic structure: passing around implicit data,
-- or short-circuiting a chain of evaluations if one fails, to choose but two.