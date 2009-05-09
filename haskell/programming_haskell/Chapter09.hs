import IO

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                  return []
                else
                  do xs <- getLine
                     return (x:xs)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"
            
beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs
                  
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (ac:acs) = do ac
                   seqn acs




-- Game of Life --
width = 50
height = 50

type Board = [Pos]

glider :: [Pos]
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells b = seqn [ writeat p "*" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                           (x + 1, y - 1), (x - 1, y),
                           (x + 1, y), (x - 1, y + 1),
                           (x, y + 1), (x + 1, y + 1)]
wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

births :: Board -> [Pos]
--births b = [(x, y) | x <- [1..width], y <- [1..height], isEmpty b (x, y), liveneighbs b (x, y) == 3]
births b = [(x, y) | (x, y) <- rmdups (concat (map neighbs b)), 
                     isEmpty b (x, y), 
                     liveneighbs b (x, y) == 3]
                     
nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life b = do cls
            showcells b
            wait 50000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n]] -- Rather unimaginative
