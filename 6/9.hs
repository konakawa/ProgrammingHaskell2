sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> a
take 0 (x:_)  = x
take n (_:xs) = take (n - 1) xs

last :: [a] -> a
last [x]    = x
last (_:xs) = last xs