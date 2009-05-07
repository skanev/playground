product' (x:xs) = x * product xs
product' []   = 1

qsort []     = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]
