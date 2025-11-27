data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

{-

assumption:
- add n (Succ m) = Succ (add n m)
- add n Zero = n

prop: add n m = add m n

n = Zero:
  add Zero m
  (add を適用)
- m
  (add n Zero = n を <- 方向に適用)
- add m Zero

n = Succ n':
  add (Succ n') m
  (add を適用)
- Succ (add n' m)
  (帰納法の仮定)
- Succ (add m n')
  (add n (Succ m) = Succ (add n m) を <- 方向に適用)
- add m (Succ n')

-}