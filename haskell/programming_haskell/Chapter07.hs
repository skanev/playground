all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) | p x       = all' p xs
              | otherwise = False
    
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) | p x       = True
              | otherwise = any' p xs

all'' :: (a -> Bool) -> ([a] -> Bool)
all'' p = foldr (&&) True . map p

any'' :: (a -> Bool) -> ([a] -> Bool)
any'' p = foldr (||) False . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) | p x       = x:takeWhile p xs
                    | otherwise = []
                    
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs
                    
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
--map' f = foldr (\x xs -> f x:xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl ((+) . (*10)) 0
--dec2int = foldl (\s n -> s * 10 + n) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

add' (x, y) = x + y
add'' x y = x + y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

map'' :: (a -> b) -> [a] -> [b]                  
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (\_ -> False) id

main = print (take 10 (iterate' (*2) 1))
--main = print (take 10 (map'' (*2) [1..]))
--main = print (uncurry' add'' (1, 5))
--main = print (curry add' 1 2)
--main = print (sse [1, 2, 3, 4, 5])
--main = print (dec2int [1, 2, 3])
--main = print (take 10 (filter' even [1..]))
--main = print (dropWhile' odd [1, 3, 5, 7, 8, 9, 11])