-- 2 rules for how functors will behave
-- fmap id        ==   id
-- fmap (f . g)   ==   fmap f . fmap g

-- Monads laws

-- The first law states that return is a left identity for (>>=)
-- return x >>= f            ===   f x
-- do y <- return x
--   f y                    ===   f x

-- The second monad law states that return is a right identity for (>>=)
-- m >>= return              ===   m
-- do y <- m
--    return y               ===   m

-- The final law is concerned with associativity
-- m >>= (\x -> f x >>= g)   ===   (m >>= f) >>= g
-- rearranging left
-- m >>= s
--   where s x = f x >>= g
-- rearranging right
-- t >>= g
--  where t = m >>= f
-- m >>= s                   ===   t >>= g