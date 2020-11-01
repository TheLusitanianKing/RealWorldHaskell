module SimpleJson (
    JValue(..),
    -- The special notation (..) that follows the name JValue indicates that
    -- we are exporting both the type and all of its constructors
    getString,
    getInt,
    getBool,
    getObject,
    getArray,
    isNull
) where

-- If we omit the exports (and the parentheses that enclose them) from a module declaration,
-- every name in the module will be exported.
-- example: module ExportEverything where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString x) = Just x
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject x) = Just x
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray l) = Just l
getArray _          = Nothing

isNull :: JValue -> Bool
isNull x = x == JNull