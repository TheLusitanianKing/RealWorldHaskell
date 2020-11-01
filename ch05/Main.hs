module Main where

import SimpleJson

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])