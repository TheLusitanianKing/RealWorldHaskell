import Control.Monad (ap, liftM)

newtype Reader e a = R { runReader :: e -> a }

instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k  = R $ \r -> runReader (k (runReader m r)) r

instance Applicative (Reader e) where
    pure = return
    (<*>) = ap

instance Functor (Reader e) where
    fmap = liftM

ask :: Reader e e
ask = R id