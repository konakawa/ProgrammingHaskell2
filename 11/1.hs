{-# LANGUAGE CPP #-}
#include "base.hs"

treeSize :: Tree a -> Int
treeSize (Node _ ts) = 1 + sum (map treeSize ts)

n :: Int
n = treeSize (gametree empty O)

treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ ts) = 1 + maximum (map treeDepth ts)

d :: Int
d = treeDepth (gametree empty O)