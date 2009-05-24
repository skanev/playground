-- The non-improved version of the PGM parser
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

data Greymap = Greymap {
    greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
    

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise = Nothing
    
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat str = case L8.readInt str of
                Nothing -> Nothing
                Just (num, rest) | num < 0   -> Nothing
                                 | otherwise -> Just (fromIntegral num, rest)
getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes num str = let count            = fromIntegral num
                       both@(prefix, _) = L.splitAt count str
                   in if L.length prefix < count
                      then Nothing
                      else Just both

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case getBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case getBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Greymap width height maxGrey bitmap, s6)
                            
main = do handle <- openFile "./sample/foo.pgm" ReadMode
          contents <- L8.hGetContents handle
          print (parseP5 contents)