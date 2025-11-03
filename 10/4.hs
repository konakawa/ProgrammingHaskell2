readNumsAndSum :: Int -> Int -> IO Int
readNumsAndSum 0 sum = return sum
readNumsAndSum n sum = do num <- readLn
                          readNumsAndSum (n - 1) (sum + num)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- readLn
           sum <- readNumsAndSum n 0
           putStrLn ("The total is " ++ show sum)