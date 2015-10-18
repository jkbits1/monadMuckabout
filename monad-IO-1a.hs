
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

-- main2 :: IO [Char] -> IO [Char]                        
main2 :: [Char] -> IO [Char]                        
main2 currentRow = do
            line <- getLine
            if null line 
                then return("bye")
                else do                
                    -- createRowIO2 '0' blankRowIO
                    newRow <- createRowIO3 (head line) currentRow -- blankRow
                    -- newRowIO <- return newRow
                    if checkForWin newRow
                        then return("You win!")
                        else main2 newRow
                        -- return newRow
            
-- main2 blankRow

checkForWin :: [Char] -> Bool
checkForWin cs = 
    let testChar = 'X' in
        head cs == testChar && (head $ drop 1 cs) == testChar 
        && (head $ drop 2 cs) == testChar

-- mainRow :: IO ()            
-- mainRow = do   line <- getLine
            -- if null line
                -- then return ()
                -- else do
                    -- putBoard $ head line
                    -- main
            

-- put row            
-- get input
-- if someinput print changed row 
-- else quit
            
          
     -- (return x) >>= f == f x
    -- m >>= return == m
    -- (m >>= f) >>= g == m >>= (\x -> f x >>= g) 
          
            