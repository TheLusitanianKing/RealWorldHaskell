module Logger (Logger, Log, runLogger, record) where

type Log = [String]

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return ($)
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

-- lifting
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i -> return (f i)

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)

charClassWordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClassWordy (c:cs) =
    charClassWordy cs >>= \ds ->
    return (c:ds)

-- As with fmap, we often use liftM in infix form.
-- An easy way to read such an expression is “apply the pure function on the left to the result of the monadic action on the right”
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

runLogger :: Logger a -> (a, Log)

record :: String -> Logger ()