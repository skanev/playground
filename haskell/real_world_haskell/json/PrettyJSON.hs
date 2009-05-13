module PrettyJSON (renderJValue) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, compact, pretty, nest)

string :: String -> Doc
string = enclose '"' '"' .hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
    where h = showHex x ""
    
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
          
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 010000)
    where d = ord c

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close .
    fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue JNull          = text "null"
renderJValue (JNumber num)  = double num
renderJValue (JString str)  = string str
renderJValue (JArray ary)   = series '[' ']' renderJValue ary
renderJValue (JObject obj)  = series '{' '}' field obj
    where field (name, val) = string name <> text ": " <> renderJValue val
    
value = renderJValue (JObject [("f", JNumber 1), ("z", JArray [JString "Foo", JBool True, JNull]), ("q", JBool True)])

main = putStrLn (nest 2 13 value)

-- TODO Rewrite Prettify.pretty in a more senseful way
-- http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html