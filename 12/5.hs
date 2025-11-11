{-

pure id <*> x   = x
x :: f a

pure (g x)      = pure g <*> pure x
g :: a -> b
x :: a

x <*> pure y    = pure (\g -> g y) <*> x
x :: f (a -> b)
y :: a

x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
x :: f (a -> b)
y :: f (b -> c)
z :: f b

-}