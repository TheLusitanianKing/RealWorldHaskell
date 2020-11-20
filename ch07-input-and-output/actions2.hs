str2message :: String -> String
str2message input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

numbers :: [Int]
numbers = [1..10]

main :: IO ()
main = do str2action "Start of the program"
          mapM_ (str2action . show) numbers
          str2action "Done!"

{-
In main, there's a call to mapM_.
This function is similar to map.
It takes a function and a list.
The function supplied to mapM_ is an I/O action that is executed for every item in the list.
mapM_ throws out the result of the function, though you can use mapM to return a list of I/O results if you want them.
-}