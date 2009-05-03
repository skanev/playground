module PrettyStub where

data Doc = ToBeDefined
           deriving (Show)
           
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined
