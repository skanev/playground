import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
--import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import System.FilePath
import Control.Monad (forM, filterM, mapM, liftM)

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

dotless :: [FilePath] -> [FilePath]
dotless (".":xs)  = dotless xs
dotless ("..":xs) = dotless xs
dotless (x:xs)    = (x:dotless xs)
dotless []        = []

lsLa :: FilePath -> IO [FilePath]
lsLa dir = do contents <- (liftM dotless) (getDirectoryContents dir)
              files <- filterM doesFileExist (map (dir </>) contents)
              directories <- filterM doesDirectoryExist (map (dir </>) contents)
              nestedFiles <- mapM lsLa directories
              return $ files ++ (concat nestedFiles)
                 
main = do dir <- getCurrentDirectory
          files <- lsLa dir
          forM files putStrLn
          return ()

--main = do dir <- getCurrentDirectory
--         getDirectoryContents dir
