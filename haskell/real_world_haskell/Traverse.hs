{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (liftM, forM)
import Control.Exception (IOException(..), handle, bracket)
import System.Directory (Permissions(..), getPermissions, getModificationTime, getDirectoryContents,
    doesDirectoryExist)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Time (ClockTime(..))
import System.IO (IOMode(..), openFile, hClose, hFileSize)

data Info = Info {
    infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO action = handle (\(_ :: IOException) -> return Nothing) (liftM Just action)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modificationTime <- maybeIO (getModificationTime path)
    return $ Info path perms size modificationTime

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
  
getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path = do 
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names



data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)
type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed
    
    walk seed (name:names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
            done@(Done _)   -> return done
            Skip seed'      -> walk seed' names
            Continue seed'
                | isDirectory info -> do
                    next <- fold seed' path'
                    case next of
                        done@(Done _) -> return done
                        seed''        -> walk (unwrap seed'') names
                | otherwise        -> walk seed' names
    walk seed _ = return (Continue seed)
                                
firstThreeRubyFiles :: Iterator [FilePath]
firstThreeRubyFiles files info
    | length files == 3     
        = Done files
    | isDirectory info && takeFileName path == ".svn"
        = Skip files
    | extension == ".rb"
        = Continue (path:files)
    | otherwise
        = Continue files
        
  where extension = takeExtension path
        path = infoPath info
    
        
main = do
    names <- foldTree firstThreeRubyFiles [] "/work/pyfmi/site/app/"
    forM names putStrLn
    return ()