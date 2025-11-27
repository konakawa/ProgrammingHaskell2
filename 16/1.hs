data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

{-

prop: add n (Succ m) = Succ (add n m)

n = Zero:
  add Zero (Succ m)
  (add を適用)
= Succ m
  (add を逆適用)
= Succ (add Zero m)

n = Succ n':
  add (Succ n') (Succ m)
  (add を適用)
= Succ (add n' (Succ m))
  (帰納法の仮定)
= Succ (Succ (add n' m))
  (add を逆適用)
= Succ (add (Succ n') m)

-}