cellNum :: Int -> Int -> [String] -> Int
cellNum colnum rownum board = 
    let height = length board
        width = length $ head board
        col = mod colnum width
        row = mod rownum height
        charat = (board !! row) !! col
    in if charat == '.' then 0 else 1

newCol :: Int -> Int -> [String] -> Char
newCol colnum rownum board = 
    let deltas = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
        neighbors = sum [cellNum (colnum + (fst delta)) (rownum + (snd delta)) board | delta <- deltas]
        self = cellNum colnum rownum board
    in 
        if ((self == 0) && (neighbors == 3)) || 
            ((self == 1) && (neighbors > 1) && (neighbors < 4))
            then 'x'
            else '.'    

newRow :: Int -> [String] -> String
newRow rownum board = 
    let width = (length $ head board) - 1
    in [newCol colnum rownum board | colnum <- [0..width]]

newBoard :: [String] -> [String]
newBoard board = 
    let height = (length board) - 1 
    in [newRow rownum board | rownum <- [0..height]]

showBoard :: [String] -> IO ()
showBoard board = putStrLn $ foldl (\l r -> l ++ "\n" ++ r) "" board

doConway :: [String] -> IO ()
doConway board = do
    showBoard board
    inCmd <- getLine
    if null inCmd 
        then doConway $ newBoard board
        else return ()

inBoard :: [String] -> IO ()
inBoard [] = do
    inRow <- getLine
    inBoard (inRow:[])
    
inBoard prevrows = do
    inRow <- getLine
    if null inRow
        then
            doConway $ reverse prevrows
        else if length inRow == length (head prevrows) 
            then inBoard $ inRow:prevrows
            else error "Last row length does not match previous."
    

main = do
    putStrLn "Welcome to Conway's Game of Life." 
    putStrLn "To begin, please enter a rectangular starting board, one line at a time:" 
    inBoard []
    