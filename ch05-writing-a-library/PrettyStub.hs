module PrettyStub where

data Doc = ToBeDefined
         deriving (Show)

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined