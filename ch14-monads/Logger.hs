module Logger (Logger, Log, runLogger, record) where

import Control.Monad (ap)

-- it's a pair, where the first element is the result of an action
-- and the second is a list of messages logged while that action was run.
-- we've wrapped the tuple in a newtype to make it a distinct type
newtype Logger a = Logger { execLogger :: (a, Log) }
type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k  = let (a, w) = execLogger m
                   n      = k a
                   (b, x) = execLogger n
               in Logger (b, w ++ x)

-- now these 2 are mandatory when using monads (Applicative and Functor)
instance Applicative Logger where
    pure = return
    (<*>) = ap
instance Functor Logger where
    fmap = liftM

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
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
    error "unterminated character class"
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