
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

processMove :: Char -> [Char] -> [Char]                                
processMove move cs =
    if move == '0'
        then 'X': (drop 1 cs)
        else if move == '1'
            then (head cs): 'X': (drop 2 cs)
            else (take 2 cs) ++ ['X']

-- processMove '0' ['_', '_', '_']        
-- processMove '1' ['_', '_', '_']        
-- processMove '2' ['_', '_', '_']        

createRowIO2 :: Char -> IO [Char] -> IO [Char]
createRowIO2 move iocs = iocs >>= 
            (\cs -> let cs2 = processMove move cs in
                        putChar (head cs2) >> 
                        putChar (head $ drop 1 cs2) >> 
                        putChar (head $ drop 2 cs2) >> 
                        return cs2)

createRowIO3 :: Char -> [Char] -> IO [Char]
createRowIO3 move cs = let cs2 = processMove move cs in
                        putChar (head cs2) >> 
                        putChar (head $ drop 1 cs2) >> 
                        putChar (head $ drop 2 cs2) >> 
                        putStrLn "" >>
                        return cs2
                        
data Board = Board {
    rowCount :: Int,
    cells :: [Char]
}          

oneRowBoard = Board { 
    rowCount = 1, 
    cells = "___" 
}              

createBoard :: [Char] -> Board            
createBoard cs = 
    Board {
        rowCount = 1,
        cells = cs 
    }

processBoardMove :: Char -> Board -> Board                                
processBoardMove move board =
    let row = cells board in
    if move == '0'
        then createBoard ('X': (drop 1 row))
        else if move == '1'
            then createBoard ((head row): 'X': (drop 2 row))
            else createBoard ((take 2 row) ++ ['X'])

processRow :: Char -> Board -> IO Board
processRow move board = 
    let row = cells board in
        let cs = processMove move row in
                        -- putChar (head cs) >> 
                        -- putChar (head $ drop 1 cs) >> 
                        -- putChar (head $ drop 2 cs) >> 
                        -- putStrLn "" >>
                        showBoard cs >>
                        return (createBoard cs)
                        
showBoard :: [Char] -> IO ()                        
showBoard cs = 
                        putChar (head cs) >> 
                        putChar (head $ drop 1 cs) >> 
                        putChar (head $ drop 2 cs) >> 
                        putStrLn "" 
                        
-- main2 :: IO [Char] -> IO [Char]                        
-- main3 :: Board -> IO Board                        
-- main3 board = 
gameLoop :: Board -> IO Board                        
gameLoop board = 
    let currentRow = cells board in
    do
        line <- getLine
        if null line 
            then return(createBoard "bye")
            else do                
                newRow <- createRowIO3 (head line) currentRow
                if checkForWin newRow
                    then return(createBoard "You win!")
                    else gameLoop (createBoard newRow)

main4 :: Board -> IO Board
main4 board = do
    showBoard (cells board)
    gameLoop board
                    
-- main2 blankRow

checkForWin :: [Char] -> Bool
checkForWin cs = 
    let testChar = 'X' in
        head cs == testChar && (head $ drop 1 cs) == testChar 
        && (head $ drop 2 cs) == testChar

            
-- put row            
-- get input
-- if someinput print changed row 
-- else quit
            
          
     -- (return x) >>= f == f x
    -- m >>= return == m
    -- (m >>= f) >>= g == m >>= (\x -> f x >>= g) 
          
            