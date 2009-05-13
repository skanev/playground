globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""           = ""

globToRegex' ('*':cs)     = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)     = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c :charClass cs
globToRegex' ('[':c:cs)     = '[' : c : charClass cs
globToRegex' ('[':_)        = error "uncompleted character class"

globToRegex' (c:cs)       = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = "\\" ++ [c]
         | otherwise           = [c]
    where regexChars = "\\().^$]|+"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"


