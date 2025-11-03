type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard' :: Int -> Board -> IO ()
putBoard' row []     = return ()
putBoard' row (x:xs) = do putRow row x
                          putBoard' (row + 1) xs

putBoard :: Board -> IO ()
putBoard = putBoard' 1