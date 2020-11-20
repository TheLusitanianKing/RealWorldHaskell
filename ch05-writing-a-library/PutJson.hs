module PutJson where

import Data.List (intercalate)
import SimpleJson

renderJValue :: JValue -> String
renderJValue (JString s)   = s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull)       = "null"
renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs l = intercalate ", " (map renderPair l)
          renderPair (k, v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray l)    = "[" ++ values l ++ "]"
    where values [] = ""
          values x = intercalate ", " (map renderJValue x)

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue