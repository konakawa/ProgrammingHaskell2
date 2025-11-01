delete :: Eq a => a -> [a] -> [a]
delete x []                 = []
delete x (y:ys) | x == y    = ys
                | otherwise = y : delete x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice (_:_) []  = False
isChoice (x:xs) ys = elem x ys && isChoice xs (delete x ys)