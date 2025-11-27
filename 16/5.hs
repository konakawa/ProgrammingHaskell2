take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n - 1) xs

drop 0 xs     = xs
drop _ []     = []
drop n (x:xs) = drop (n - 1) xs

{-

prop: take n xs ++ drop n xs = xs

xs = []
  任意の n について
    take n [] ++ drop n []
    (take を適用)
  = [] ++ drop n []
    (drop を適用)
  = [] ++ []
    (++ を適用)
  = []

xs = x:xs':
  n = 0:
    take 0 (x:xs') ++ drop 0 (x:xs')
    (take を適用)
  = [] ++ drop 0 (x:xs')
    (drop を適用)
  = [] ++ x:xs'
    (++ を適用)
  = x:xs'

  n > 0:
    take n (x:xs') ++ drop n (x:xs')
    (take を適用)
  = (x : take (n - 1) xs') ++ drop n (x:xs')
    (drop を適用)
  = (x : take (n - 1) xs') ++ drop (n - 1) xs'
    (++ を適用)
  = x : (take (n - 1) xs' ++ drop (n - 1) xs')
    (帰納法の仮定)
  = x : xs'

-}