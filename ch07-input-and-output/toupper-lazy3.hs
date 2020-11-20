import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do 
       inh <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inh)