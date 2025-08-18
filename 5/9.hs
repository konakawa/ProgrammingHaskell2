scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [xs !! n * ys !! n | n <- [0..length xs - 1]]