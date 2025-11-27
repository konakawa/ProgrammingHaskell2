data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-

prop: fmap id t = id t

t = Leaf x:
  fmap id (Leaf x)
  (fmap を適用)
= Leaf (id x)
  (id を適用)
= Leaf x
  (id を逆適用)
= id (Leaf x)

t = Node l r:
  fmap id (Node l r)
  (fmap を適用)
= Node (fmap id l) (fmap id r)
  (帰納法の仮定 x2)
= Node (id l) (id r)
  (id を適用 x2)
= Node l r
  (id を逆適用)
= id (Node l r)


prop: fmap (g . h) t = fmap g (fmap h t)

t = Leaf x:
  fmap (g . h) (Leaf x)
  (fmap を適用)
= Leaf (g (h x))
  (fmap を逆適用)
= fmap g (Leaf (h x))
  (fmap を逆適用)
= fmap g (fmap h (Leaf x))

t = Node l r:
  fmap (g . h) (Node l r)
  (fmap を適用)
= Node (fmap (g . h) l) (fmap (g . h) r)
  (帰納法の仮定 x2)
= Node (fmap g (fmap h l)) (fmap g (fmap h r))
  (fmap を逆適用)
= fmap g (Node (fmap h l) (fmap h r))
  (fmap を逆適用)
= fmap g (fmap h (Node l r))

-}