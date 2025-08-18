{-

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palidrome :: [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> b) -> a -> b
twice f x = f (f x)

-}