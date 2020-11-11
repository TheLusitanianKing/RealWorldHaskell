module GlobRegex (
    globToRegex,
    matchesGlob
) where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' = undefined

matchesGlob = undefined