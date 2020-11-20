import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do putStrLn "Is green your favorite colour?"
             inpStr <- getLine
             return ((toUpper . head $ inpStr) == 'Y')

isGreen2 :: IO Bool
isGreen2 = putStrLn "Is green your favorite colour?" >>
           getLine >>=
           (\inpStr -> return ((toUpper . head $ inpStr) == 'Y'))