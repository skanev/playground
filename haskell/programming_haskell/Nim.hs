import Data.Char

type Pos = (Int, Int)

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs
                  
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (ac:acs) = do ac
                   seqn acs
                   
type Board = [Int]
data Player = First | Second

other :: Player -> Player
other First  = Second
other Second = First

name :: Player -> String
name First  = "N1"
name Second = "N2"

initial :: Board
initial = [5, 4, 3, 2, 1]

showboard :: Board -> IO ()
showboard b = do cls
                 seqn [writeat (1, y) (show y ++ ": " ++ line t) | (y, t) <- zip [1..5] b]
                    
line :: Int -> String
line tokens = (replicate tokens '*') ++ (replicate 5 ' ')

move :: Board -> Player -> IO ()
move board player = do cls
                       showboard board
                       if complete board then
                             writeat (1, 6) ("Winner: " ++ name (other player) ++ "\n")
                           else do writeat (1, 6) (name player ++ " (row-number): ")
                                   input <- getLine
                                   case process board input of
                                        [row, number] -> move (takeAt board (row - 1) number) (other player)
                                        [] -> do beep
                                                 move board player

complete :: Board -> Bool
complete [0, 0, 0, 0, 0] = True
complete _               = False

chr2num :: Char -> Int
chr2num c | (ord '0' <= ord c) && (ord c <= ord '9') = ord c - ord '0'
          | otherwise                                = -1

takeAt :: Board -> Int -> Int -> Board
takeAt (x:xs) 0       n = x - n:xs
takeAt (x:xs) (r + 1) n = x:takeAt xs r n

at :: Board -> Int -> Int          
at board row = board !! (row - 1)

feasible :: Board -> Int -> Int -> Bool
feasible b r n = and [r >= 1, r <= 5, n >= 1, n <= (at b r)]

process :: Board -> String -> [Int]
process b [r, '-', n] | feasible b (chr2num r) (chr2num n) = [(chr2num r), (chr2num n)] 
process b _ = []

f = main
main = move initial First