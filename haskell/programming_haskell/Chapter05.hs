squareSum :: Int -> Int
squareSum n = sum [x ^ 2 | x <- [1..n]]

replicate2 :: Int -> a -> [a]
replicate2 n x = [x | _ <- [1..n]]

pairs xs = zip xs (tail xs)

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x ^ 2 + y ^ 2 == z ^ 2]

main = print (take 10 (pyths 10))