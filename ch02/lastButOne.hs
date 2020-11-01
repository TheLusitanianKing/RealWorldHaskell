lastButOne :: [a] -> a
lastButOne x | length x <= 1 = error "impossible" 
lastButOne (x:y:[]) = x
lastButOne (x:y:xs) = lastButOne (y:xs)