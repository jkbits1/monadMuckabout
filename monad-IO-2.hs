
-- (>>) :: (Monad m) => m a -> m b -> m b
-- x >> y = x Prelude.>>= (\_ -> y)

-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- x >>= f = f x
-- xs >>= f = join (fmap f xs)

-- instance Monad [] where
    -- return x = [x]
    -- xs >>= f = concat (map f xs)

-- main = do   putStrLn "Hello, who are you?"
            -- name <- getLine
            -- putStrLn ("Hi" ++ name)

main :: IO ()            
main = do   line <- getLine
            if null line
                then return ()
                else do
                    -- putStrLn $ line
                    putBoard $ head line
                    main

-- putBoard :: IO ()                    
putBoard :: Char -> IO ()                    
putBoard c =  
            -- putStrLn "_|_|_" >>
            putRow      c     >>
            putStrLn    "_|_|_" >>
            putStrLn    "_|_|_" 
            
putBoard2 :: Char -> IO Char                   
putBoard2 c =  
            -- putStrLn "_|_|_" >>
            putRow      c     >>
            putStrLn    "_|_|_" >>
            -- putStrLn    "_|_|_" 
            putRowTest
            
putRow :: Char -> IO ()
putRow c =  if c == '0' 
                then putStrLn "X|_|_" 
                else putStrLn "_|X|_" 
            
putRowTest :: IO Char
putRowTest = putStrLn "X|_|_" >> return 'c'

putIOChar :: IO Char -> IO ()
putIOChar ioc = ioc >>= (\c -> putChar c >> putStrLn "")

-- putIOChar putRowTest
                    
putRowList :: IO [Char]
putRowList = putStrLn "X|_|_" >> return ['c']

putIOCharList :: IO [Char] -> IO ()
putIOCharList iocs = iocs >>= (\cs -> putChar (head cs) >> putStrLn "")

-- putIOCharList putRowList

blankRowIO :: IO [Char]
blankRowIO = return ['_', '_', '_']

blankRow :: [Char]
blankRow = ['_', '_', '_']


-- putIOCharList blankRowIO

putRowIO :: IO [Char] -> IO [Char]
putRowIO iocs = iocs >>= 
            (\cs -> putChar (head cs) >> 
                        -- putChar (head cs) >> 
                            -- putChar (head cs) >> 
                                return cs)

createRowIO :: Char -> IO [Char] -> IO [Char]
createRowIO move iocs = iocs >>= 
            (\cs -> if move == '0'
                        then    putChar 'X' >> 
                                putChar (head $ drop 1 cs) >> 
                                putChar (head $ drop 2 cs) >> 
                                return cs
                        else    putChar (head cs) >> 
                                putChar 'X' >> 
                                putChar (head $ drop 2 cs) >> 
                                return cs)
                                
-- createRowIO '0' blankRowIO
-- createRowIO '1' blankRowIO

-- processMove :: Char -> [Char] -> [Char]                                
-- processMove move cs 
    -- | move == '0'   = 'X': (drop 1 cs)
    -- | move == '1'   = (head cs): 'X': (drop 2 cs)
    -- | otherwise     = (take 2 cs) ++ ['X']
    -- if move == '0'
        -- then 'X': (drop 1 cs)
        -- else if move == '1'
            -- then (head cs): 'X': (drop 2 cs)
            -- else (take 2 cs) ++ ['X']

-- processMove '0' ['_', '_', '_']        
-- processMove '1' ['_', '_', '_']        
-- processMove '2' ['_', '_', '_']        

-- createRowIO2 :: Char -> IO [Char] -> IO [Char]
-- createRowIO2 move iocs = iocs >>= 
            -- (\cs -> let cs2 = processMove move cs in
                        -- putChar (head cs2) >> 
                        -- putChar (head $ drop 1 cs2) >> 
                        -- putChar (head $ drop 2 cs2) >> 
                        -- return cs2)

-- createRowIO3 :: Char -> [Char] -> IO [Char]
-- createRowIO3 move cs = let cs2 = processMove move cs in
                        -- putChar (head cs2) >> 
                        -- putChar (head $ drop 1 cs2) >> 
                        -- putChar (head $ drop 2 cs2) >> 
                        -- putStrLn "" >>
                        -- return cs2
                        
data Board = Board {
    rowCount    :: Int,
    cells       :: [Char],
    message     :: [Char]
}          

oneRowBoard = Board { 
    rowCount = 1, 
    cells = "___",
    message = ""
}              

createBoard :: [Char] -> Board            
createBoard cs = 
    Board {
        rowCount = 1,
        cells = cs,
        message = ""
    }
    
newMoveBoard :: Board -> [Char] -> Board
newMoveBoard board cs = 
    Board {
        rowCount = (rowCount board),
        cells = cs,
        message = (message board)
    }

newMessageBoard :: Board -> [Char] -> Board            
newMessageBoard board message = 
    Board {
        rowCount = (rowCount board),
        cells = (cells board),
        message = message
    }

processBoardMove :: Char -> Board -> Board                                
processBoardMove move board
    | move == '0'   = newMoveBoard board ('X': (drop 1 row))
    | move == '1'   = newMoveBoard board ((head row): 'X': (drop 2 row))
    | otherwise     = newMoveBoard board ((take 2 row) ++ ['X'])
    where row = cells board 
         
processBoard :: Char -> Board -> IO Board
processBoard move board = return (processBoardMove move board)
                        
-- showBoard :: [Char] -> IO ()                        
-- showBoard cs = putChar (head cs) >> 
showBoard :: Board -> IO ()                        
showBoard board = putChar (head cs) >> 
                    putChar (head $ drop 1 cs) >> 
                    putChar (head $ drop 2 cs) >> 
                    putStrLn "" 
                    where cs = cells board
                        
-- main2 :: IO [Char] -> IO [Char]                        
gameLoop :: Board -> IO Board                        
gameLoop board = 
    do
        line <- getLine
        -- case line of 
        if null line 
            then return(newMessageBoard board "bye")
            else do                
                 newBoard <- processBoard (head line) board
                 showBoard newBoard
                 if checkForWin newBoard
                    then return (newMessageBoard newBoard "You win!")
                    else gameLoop newBoard

play :: Board -> IO ()
play board = do
    showBoard board
    finalBoard <- gameLoop board
    putStrLn (message finalBoard)
                    
-- play oneRowBoard
-- main2 blankRow

checkForWin :: Board -> Bool
checkForWin board = 
    let cs = cells board in
    let testChar = 'X' in
        head cs == testChar 
            && (head $ drop 1 cs) == testChar 
            && (head $ drop 2 cs) == testChar
            
-- checkForWin :: [Char] -> Bool
-- checkForWin cs = 
    -- let testChar = 'X' in
        -- head cs == testChar && (head $ drop 1 cs) == testChar 
        -- && (head $ drop 2 cs) == testChar

-- put row            
-- get input
-- if someinput print changed row 
-- else quit
            
          
     -- (return x) >>= f == f x
    -- m >>= return == m
    -- (m >>= f) >>= g == m >>= (\x -> f x >>= g) 
          
            