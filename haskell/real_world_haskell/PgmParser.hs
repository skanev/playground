-- The non-improved version of the PGM parser
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr, isDigit)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Word (Word8)
import Control.Applicative ((<$>))

data Greymap = Greymap {
    greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Integer
} deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser init =
  case runParse parser (ParseState init 0) of
    Left error        -> Left error
    Right (result, _) -> Right result

parseByte :: Parse Word8
parseByte = 
  getState ==> \state ->
      case L.uncons (string state) of
          Nothing -> bail "not enough input"
          Just (char, remainder) -> putState newState ==> \_ -> identity char
              where newState = state { string = remainder, offset = newOffset }
                    newOffset = (offset state) + 1
              
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

(==>) :: Parse a -> (a -> Parse b) -> Parse b
first ==> second = Parse  result
  where result init =
          case runParse first init of
                Left msg -> Left msg
                Right (result, newState) -> runParse (second result) newState

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
    "byte offset " ++ show (offset s) ++ ": " ++ err
    
instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte
                      
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
                if mp == Just True
                  then parseByte ==> \b -> (b:) <$> parseWhile p
                  else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
             then bail "no more input"
             else let n = read digits
                  in if n < 0
                       then bail "Integer overflow"
                       else identity n
                       
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _    = identity ()
assert False msg = bail msg

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset st + fromIntegral (L.length h), string = t }
  in putState st' ==>&
      assert (L.length h == n') "end of input" ==>&
      identity h
            
parseRawPGM =
  parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "Bad header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey -> parseByte ==>&
    assert (maxGrey <= 255) "Max grey is far too big" ==>&
    parseBytes (width * height) ==> \bytes ->
    identity $ Greymap width height maxGrey bytes
  where notWhite = (`notElem` " \r\n\t")

main = do handle <- openFile "./sample/foo.pgm" ReadMode
          contents <- L8.hGetContents handle
          print (parse parseRawPGM contents)