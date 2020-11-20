-- myShow = show -- do not compile

myShow2 value = show value

myShow3 :: (Show a) => a -> String
myShow3 = show