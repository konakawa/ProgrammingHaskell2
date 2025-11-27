[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

{-

prop: xs ++ [] = xs

xs = []:
  [] ++ []
  (++ を適用)
= []

xs = x:xs':
  (x : xs') ++ []
  (++ を適用)
= x : (xs' ++ [])
  (帰納法の仮定)
= x : xs'

prop: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

xs = []:
  [] ++ (ys ++ zs)
  (++ を適用)
= ys ++ zs
  (++ を逆適用)
= ([] ++ ys) ++ zs

xs = x:xs':
  (x : xs') ++ (ys ++ zs)
  (++ を適用)
= x : (xs' ++ (ys ++ zs))
  (帰納法の仮定)
= x : ((xs' ++ ys) ++ zs)
  (++ を逆適用)
= (x : (xs' ++ ys)) ++ zs
  (: を適用)
= (x : xs' ++ ys) ++ zs

-}