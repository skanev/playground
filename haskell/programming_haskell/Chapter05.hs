squareSum :: Int -> Int
squareSum n = sum [x ^ 2 | x <- [1..n]]

replicate2 :: Int -> a -> [a]
replicate2 n x = [x | _ <- [1..n]]

pairs xs = zip xs (tail xs)

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x ^ 2 + y ^ 2 == z ^ 2]

perfects :: Int -> [Int]
perfects x = [n | n <- [1..x], (sum (factors n)) == n]
    where factors n = [a | a <- [1..(n-1)], n `mod` a == 0]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys]

main = print (scalarproduct [1, 2, 3] [4, 5, 6])
--main = print (perfects 500)
--main = print (positions False [True, False, False, True, False])
--main = print (take 10 (pyths 10))