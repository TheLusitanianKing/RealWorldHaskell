import qualified Data.ByteString.Lazy as L
import Data.Int (Int64(..))

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
} deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- remember that the newtype definition is just a compile-time wrapper around a function
-- so it has no run-time overhead
-- when we want to use the function, we will apply the runParser accessor
newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}