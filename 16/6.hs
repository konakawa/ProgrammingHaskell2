data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

{-

prop: leaves t = 1 + nodes t

t = Leaf n:
  leaves (Leaf n)
  (leaves を適用)
= 1
= 1 + 0
  (nodes を逆適用)
= 1 + nodes (Leaf n) 

t = Node l r:
  leaves (Node l r)
  (leaves を適用)
= leaves l + leaves r
  (帰納法の仮定)
= 1 + nodes l + 1 + nodes r
= 1 + 1 + nodes l + nodes r
  (nodes を逆適用)
= 1 + nodes (Node l r)

-}