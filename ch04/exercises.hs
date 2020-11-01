safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit l  = Just (getAllButLast l)
    where getAllButLast [x]      = []
          getAllButLast (x:y:[]) = [x]
          getAllButLast (x:xs)   = x : getAllButLast xs

-- Write a function splitWith that acts similarly to words,
-- but takes a predicate and a list of any type,
-- and splits its input list on every element for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f l  = doSplitWith f l [] []
    where doSplitWith _ [] globalAcc acc           = globalAcc ++ [acc]
          doSplitWith f (x:xs) globalAcc acc | f x = doSplitWith f xs globalAcc (acc ++ [x])
          doSplitWith f (_:xs) globalAcc acc       = doSplitWith f xs (globalAcc ++ [acc]) []