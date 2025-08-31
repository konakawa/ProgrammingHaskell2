and :: [Bool] -> Bool
and []     = True
and (x:xs) = if x then and xs else False

concat :: [[a]] -> [a]
concat []     = []
concat (l:ls) = l ++ concat ls

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ []                 = False
elem x (y:ys) | x == y    = True
              | otherwise = elem x ys