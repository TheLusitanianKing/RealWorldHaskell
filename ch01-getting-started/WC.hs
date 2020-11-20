-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

-- number of lines
--main = interact wordCount
--    where wordCount input = show (length (lines input)) ++ "\n"

-- number of words
--main = interact wordCount
--    where wordCount input = show (length (words input)) ++ "\n"

main = interact wordCount
    where wordCount input = show (length input) ++ "\n"