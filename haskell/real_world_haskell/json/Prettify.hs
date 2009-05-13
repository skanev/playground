module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line            = Char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty         -> transform ds
                  Char c        -> c : transform ds
                  Text s        -> s ++ transform ds
                  Line          -> '\n' : transform ds
                  a `Concat` b  -> transform (a:b:ds)
                  _ `Union` b   -> transform (b:ds)


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds
                  
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
            case d of
                Empty           -> best col ds
                Char c          -> c : best (col + 1) ds
                Text s          -> s ++ best (col + length s) ds
                Line            -> '\n' : best 0 ds
                a `Concat` b    -> best col (a:b:ds)
                a `Union` b     -> nicest col (best col (a:ds)) (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col


nest :: Int -> Int -> Doc -> String
nest shiftwidth width x = best 0 0 [x]
     where best i len (d:ds) =
             case d of
                 Empty           -> best i len ds
                 Char c          -> c:best indent (len + 1) ds
                     where indent = 
                             case c of
                                 '{' -> i + 1
                                 '}' -> i - 1
                                 '[' -> i + 1
                                 ']' -> i - 1
                                 otherwise -> i
                 Text str        -> str ++ best i (len + (length str)) ds
                 Line            -> "\n" ++ shift ++ best i (length shift) ds
                     where shift = (replicate (i * shiftwidth) ' ')
                 a `Concat` b    -> best i len (a:b:ds)
                 a `Union` b     -> nicest len (best i len (a:ds)) (best i len (b:ds))
           best _ _ _ = ""
           nicest pad x y | (width - pad) `fits` x = x
                          | otherwise                = y
                          -- where least = min width pad



fits :: Int -> String -> Bool
w `fits` _ | w < 0  = False
w `fits` ""         = True
w `fits` ('\n':_)   = True
w `fits` (c:cs)     = (w - 1) `fits` cs
          