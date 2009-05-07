halve :: [a] -> ([a], [a])
halve l = splitAt ((length l) `div` 2) l

safetail1 :: [a] -> [a]
safetail1 x = if (null x) then [] else tail x

safetail2 :: [a] -> [a]
safetail2 l | null l    = []
            | otherwise = tail l
            
safetail3 :: [a] -> [a]
safetail3 (x:xs) = xs
safetail3 _      = []