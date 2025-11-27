-- モナド則
-- return x >>= f   = f x
-- mx >>= return    = mx
-- (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [g x | g <- gs, x <- xs]

instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]

  -- return = pure

{-

prop: return x >>= f = f x

  return x >>= f
  (return を適用)
= [x] >>= f
  (>>= を適用)
= [y | x <- [x], y <- f x]
= f x


prop: mx >>= return = mx

  mx >>= return
  (>>= を適用)
= [y | x <- mx, y <- return x]
  (return を適用)
= [y | x <- mx, y <- [x]]
= mx


prop: (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

  (mx >>= f) >>= g
  (>>= を適用)
= [y | x <- mx, y <- f x] >>= g
  (>>= を適用)
= [z | y <- [y | x <- mx, y <- f x], z <- g y]
= [z | x <- mx, y <- f x, z <- g y]

  mx >>= (\x -> (f x >>= g))
  (>>= を適用)
= [z | x <- mx, z <- (\x -> (f x >>= g)) x]
= [z | x <- mx, z <- f x >>= g]
  (>>= を適用)
= [z | x <- mx, z <- [z | y <- f x, z <- g y]]
= [z | x <- mx, y <- f x, z <- g y]

-}