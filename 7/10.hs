altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . map (\n -> if n > 9 then n - 9 else n) . reverse . altMap id (*2) . reverse