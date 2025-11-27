all :: (a -> Bool) -> [a] -> Bool
all p []     = True
all p (x:xs) = p x && all p xs

{-

prop: all (== x) (replicate n x)

n = 0:
  all (== x) (replicate 0 x)
  (replicate を適用)
= all (== x) []
  (all を適用)
= True

n = n' + 1
  all (== x) (replicate (n' + 1) x)
  (replicate を適用)
= all (== x) (x : replicate n' x)
  (all を適用)
= (x == x) && all (== x) (replicate n' x)
  (x == x を評価)
= True && all (== x) (replicate n' x)
  (&& を評価)
= all (== x) (replicate n' x)
  (帰納法の仮定)
= True

-}