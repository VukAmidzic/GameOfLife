import Data.MemoTrie

data State = Alive | Dead deriving (Eq, Show)
type Pos = (Integer, Integer) 
type Grid = Pos -> State

evolution :: Grid -> Integer -> Grid
evolution grid = memo2 go
    where 
        go 0 p = grid p
        go n p = next (go (n-1) p) (map (go (n-1)) (neighbors p)) 

next :: State -> [State] -> State
next Alive adj 
        | count Alive adj < 2 = Dead
        | count Alive adj > 3 = Dead
        | otherwise = Alive
next Dead adj
        | count Alive adj == 3 = Alive
        | otherwise = Dead
            
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
        
neighbors :: Pos -> [Pos]
neighbors (x,y) = 
    [(x+n, y+m) | m <- [-1,0,1], n <- [-1,0,1], (n,m) /= (0,0), x+n <= 10, y+m <= 10, x+n > 0, y+m > 0]

drawGen gen = 
    map (drawLine gen) [1..10]
    
drawLine :: Grid -> Integer -> String
drawLine gen y = 
    concat (map (drawCell gen y) [1..10])
    
drawCell gen y x = 
    case (gen (x,y)) of 
        Alive -> ['X']
        Dead -> [' ']
    
main :: IO()
main = do
    let grid (5,4) = Alive
        grid (6,4) = Alive
        grid (5,5) = Alive
        grid (6,5) = Alive
        grid (_,_) = Dead
    mapM_ print (drawGen $ evolution grid 6)
    
