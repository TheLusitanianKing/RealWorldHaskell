comprehensive xs ys = [(x, y) | x <- xs, y <- ys]

monadic xs ys = do { x <- xs; y <- ys; return (x, y) }

blockyDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)
-- for every element in the list xs, the rest of the function is evaluated once
-- with x bound to a different value from the list each time.
-- then for every element in the list ys, the remainder of the function is evaluated once,
-- with y bound to a different value from the list each time.

-- this highlights an important fact about monads: you cannot predict
-- how a block of monadic code will behave unless you know what monad it will execute in

blockyPlain xs ys =
    xs >>=
    \x -> ys >>=
    \y -> return (x, y)

blockyPlainReloaded xs ys =
    concat (map (\x ->
                 concat (map (\y ->
                              return (x, y))
                         ys))
            xs)