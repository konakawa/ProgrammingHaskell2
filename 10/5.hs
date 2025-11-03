adder :: IO ()
adder = do putStr "How many numbers? "
           n <- readLn
           nums <- sequence (replicate n readLn)
           putStrLn ("The total is " ++ show (sum nums))