mySecond :: [a] -> a
mySecond []      = error "list is empty"
mySecond [_]     = error "list is too short"
mySecond (_:x:_) = x

safeSecond :: [a] -> Maybe a
safeSecond []      = Nothing
safeSecond [_]     = Nothing
safeSecond (_:x:_) = Just x

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing