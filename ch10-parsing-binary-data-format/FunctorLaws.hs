-- our first rule is that a functor must preserve identity
-- applying fmap id to a value should give us back an identical value.
-- fmap id ==  id

-- our second rule is that functors must be composable
-- composing two uses of fmap should give the same result as one fmap with the same functions composed.
-- fmap (f . g)  ==  fmap f . fmap g