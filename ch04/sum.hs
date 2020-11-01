mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc+x) xs
          helper acc _      = acc

-- sum with foldl
foldlSum = foldl (+) 0