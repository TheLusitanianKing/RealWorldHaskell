-- {-# LANGUAGE FlexibleInstances #-}

-- instance Functor (Either Int) where
--     fmap _ (Left n) = Left n
--     fmap f (Right r) = Right (f r)
-- although here it does work as (Either a) is a instance of Functor already