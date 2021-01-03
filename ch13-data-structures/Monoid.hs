import Prelude hiding (Monoid)
import DList

class Monoid a where
    mempty  :: a                -- the identity
    mappend :: a -> a -> a      -- associative binary operator

instance Monoid [a] where
    mempty  = []
    mappend = (++)

instance Monoid (DList a) where
    mempty = empty
    mappend = append