safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs | null xs   = []
             | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC []     = []
safetailC (x:xs) = xs