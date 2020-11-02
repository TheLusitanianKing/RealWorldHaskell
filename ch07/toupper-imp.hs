import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh
    
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           -- In Haskell, return is the opposite of <-.
           -- That is, return takes a pure value and wraps it inside IO.
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh
                   -- When reading and writing from a Handle that corresponds to a file on disk,
                   -- the operating system maintains an internal record of the current position.
                   -- Each time you do another read, the operating system returns the next chunk of data
                   -- that begins at the current position, and increments the position to reflect the data that you read.