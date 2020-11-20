import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

asInt xs = loop 0 xs

-- tail-recursive function (because the last thing that loop does is simply call itself)
loop acc []     = acc -- base case (or terminating case)
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs -- recursive case (or inductive case)