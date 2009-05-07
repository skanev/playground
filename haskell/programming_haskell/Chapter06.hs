(^^) :: Int -> Int -> Int
x ^^ 0       = 1
x ^^ (a + 1) = x * (x ^ a)

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and xs

concat' :: [[a]] -> [a]
concat' xss = [ x | xs <- xss, x <- xs]

replicate' :: Int -> a -> [a]
replicate' 0 _       = []
replicate' (n + 1) x = x:replicate n x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0       = x
(x:xs) !!! (a + 1) = xs !!! a

elm :: Eq a => a -> [a] -> Bool
e `elm` []                 = False
e `elm` (x:xs) | e == x    = True
               | otherwise = e `elm` xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve l = splitAt ((length l) `div` 2) l

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [a] = [a]
mergesort xs  = merge (mergesort left) (mergesort right)
    where (left, right) = halve xs

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Num a => Int -> [a] -> [a]
take' 0 _            = []
take' _ []           = []
take' (n + 1) (x:xs) = x:(take' n xs)

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs

--main = print (take' 12 [1, 2, 3, 4, 5])
--main = print (mergesort [5, 4, 2, 8, 11, 3, 23, 9])
--main = print (halve [1])
--main = print (merge [2, 5, 6] [1, 3, 4])
--main = print (5 `elm` [1, 2, 3])
--main = print ([1, 2, 3, 4] !!! 1)
--main = print (concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
--main = print (2 ^ 8)