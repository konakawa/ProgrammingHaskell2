data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeatT :: a -> Tree a
repeatT x = t where t = Node t x t

takeT :: Int -> Tree a -> Tree a
takeT 0 _            = Leaf
takeT _ Leaf         = Leaf
takeT n (Node l x r) = Node (takeT (n - 1) l) x (takeT (n - 1) r)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT