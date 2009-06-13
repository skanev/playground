{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import System.Directory (Permissions(..), getModificationTime,
    getPermissions, getDirectoryContents, doesDirectoryExist)
import System.Time (ClockTime(..))
import System.FilePath ((</>), takeExtension)
import Control.Exception
import System.IO (IOMode(..), hClose, hFileSize, openFile)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents top = do
    names <- getDirectoryContents top
    let proper = filter (`notElem` [".", ".."]) names
    paths <- forM proper $ \name -> do
        let path = top </> name
        isDir <- doesDirectoryExist path
        if isDir
            then getRecursiveContents path
            else return [path]
    return (concat paths)



simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)



type Predicate = FilePath -> Permissions -> Maybe Integer -> ClockTime -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = do
    paths <- getRecursiveContents path
    filterM check paths
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: IOException) -> return Nothing) $ do
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

type InfoP a = FilePath -> Permissions -> Maybe Integer -> ClockTime -> a

pathP :: InfoP FilePath
pathP p _ _ _ = p

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

liftPath :: (FilePath -> a) -> InfoP a
liftPath f = \n _ _ _ -> f n

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP o f r = \n p s m -> f n p s m `o` r

lift2P :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
lift2P o l r = \n p s m -> l n p s m `o` r n p s m

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = lift2P (&&)
orP = lift2P (||)
    
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

(==?) = equalP
(&&?) = andP
(>?) = greaterP
(<?) = lesserP

infix 4 ==?
infixr 3 &&?
infix 4 >?



main = do
    paths <- betterFind (sizeP >? 20 &&? sizeP <? 30 &&? liftPath takeExtension ==? ".rb") "/work/pyfmi/site/app/helpers"
    forM paths $ \p -> do
        putStrLn p