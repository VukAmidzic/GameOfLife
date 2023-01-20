data CellState = Alive | Dead 
data CellPosition = CellPosition Integer Integer
type Generation = CellPosition -> CellState

isAlive :: CellState -> Bool
isAlive Alive = True
isAlive Dead = False

cellNeigh :: CellPosition -> [CellPosition]
cellNeigh (CellPosition x y) = 
    [(CellPosition (x-1) (y-1)), (CellPosition x (y-1)),  (CellPosition (x+1) (y-1)), (CellPosition (x+1) y),
  (CellPosition (x+1) (y+1)), (CellPosition x (y+1)), (CellPosition (x-1) (y+1)), (CellPosition (x-1) y)]
    
numAliveNeigh :: Generation -> CellPosition -> Int
numAliveNeigh gen pos = 
    length (filter isAlive (map gen (cellNeigh pos))) 
    
evolution :: Generation -> Generation 
evolution gen pos = 
    case (numAliveNeigh gen pos) of
    2 -> if (isAlive (gen pos)) then Alive else Dead
    3 -> Alive
    _ -> Dead
    
    
drawGen gen = map (drawCol gen) [1..10]

drawCol :: Generation -> Integer -> String
drawCol gen y = concat (map (drawCell gen y) [1..10])

drawCell gen y x = 
    case (gen (CellPosition x y)) of 
    Alive -> ['X']
    Dead -> [' ']
    
