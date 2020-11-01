import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _               -> putStrLn "error: exactly two arguments needed"
          myFunction = transposeFile -- put our function here (id is the identify function)

-- Using the command framework from the section called “A simple command line framework”,
-- write a program that prints the first word of each line of its input
firstWordOfEachLine :: [String] -> [String]
firstWordOfEachLine [] = []
firstWordOfEachLine (x:xs) = [firstWordOfLine x] ++ firstWordOfEachLine xs
    where firstWordOfLine x = if null $ words x then "" else head $ words x

firstWordOfEachLineFile :: String -> String
firstWordOfEachLineFile x = unlines $ firstWordOfEachLine $ lines x

-- Write a program that transposes the text in a file.
-- For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n"
transposeFile :: String -> String
transposeFile x = unlines $ transpose $ lines x

transpose :: [String] -> [String]
transpose x | hasAnEmptyList x = []
transpose x                    = (map head x) : transpose (map tail x)

hasAnEmptyList :: [String] -> Bool
hasAnEmptyList []     = False
hasAnEmptyList ([]:_) = True
hasAnEmptyList (x:xs) = hasAnEmptyList xs