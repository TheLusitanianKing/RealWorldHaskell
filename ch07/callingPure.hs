name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine -- When you're working in a do block, you use <- to get results from IO actions
       -- and let to get results from pure code
       let outStr = name2reply inpStr -- When used in a do block, you should not put in after your let statement
       putStrLn outStr
       -- could have been: putStrLn $ name2reply inpStr