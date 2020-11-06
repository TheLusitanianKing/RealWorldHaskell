returnTest :: IO ()
returnTest =
    do one <- return 1 -- return does not have to be at the end (usually is though)
       let two = 2
       putStrLn $ show (one + two)