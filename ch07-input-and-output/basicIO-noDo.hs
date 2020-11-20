-- FROM
--main = do
--       putStrLn "Greetings!  What is your name?"
--       inpStr <- getLine
--       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

-- TO
main :: IO ()
main = putStrLn "Greetings! What is your name?" >>
       getLine >>=
       (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")

{-
The >> operator sequences two actions together: the first action is performed, then the second.
The result of the computation is the result of the second action. The result of the first action is thrown away. 
-}

{-
The >>= operator runs an action, then passes its result to a function that returns an action.
That second action is run as well, and the result of the entire expression is the result of that second action.
-}