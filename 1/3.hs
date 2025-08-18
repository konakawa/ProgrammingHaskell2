myProduct []     = 1
myProduct (n:ns) = n * myProduct ns

{-
   product [2, 3, 4]
-> 2 * product [3, 4]
-> 2 * (3 * product [4])
-> 2 * (3 * (4 * product []))
-> 2 * (3 * (4 * 1))
-> 2 * (3 * 4)
-> 2 * 12
-> 24
-}