euclid :: Int -> Int -> Int
euclid m n | m == n    = m
           | m < n     = euclid n m
           | otherwise = euclid n (m - n)