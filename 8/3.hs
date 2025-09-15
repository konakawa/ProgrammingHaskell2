data Tree a = Leaf a | Node (Tree a) (Tree a)

numOfLeaves :: Tree a -> Int
numOfLeaves (Leaf _)   = 1
numOfLeaves (Node l r) = numOfLeaves l + numOfLeaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = balanced l && balanced r && abs (numOfLeaves l - numOfLeaves r) <= 1