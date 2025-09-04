dec2int :: [Int] -> Int
dec2int = foldl (\sum -> \dec -> 10 * sum + dec) 0