data Op = Add | Sub | Mul | Div | Pow
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Pow x y = y >= 0 && (y == 0 || x /= 0)

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                      brak (Val n) = show n
                      brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Pow]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l        <- exprs ls,
                 r        <- exprs rs,
                 e        <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- main :: IO ()
-- main = print (solutions [1, 3, 7, 10, 25, 50] 765)

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx       <- results ls,
                     ry       <- results rs,
                     res      <- combine' lx ry]

complexity :: Expr -> Int
complexity (Val _)     = 0
complexity (App _ l r) = 1 + complexity l + complexity r

insert :: (a -> a -> Bool) -> a -> [a] -> [a]
insert _ x [] = [x]
insert cmp x (y:ys)
  | cmp x y   = x : y : ys
  | otherwise = y : insert cmp x ys

sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy cmp = foldr (insert cmp) []

findClosest :: Int -> [Result] -> [Expr]
findClosest _ [] = []
findClosest target rs =
  let minDiff = minimum [abs (target - m) | (_, m) <- rs]
      closest = [e | (e, m) <- rs, abs (target - m) == minDiff]
  in sortBy (\e1 e2 -> complexity e1 < complexity e2) closest

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  let allResults = [(e, m) | ns' <- choices ns, (e, m) <- results ns']
      exact = [e | (e, m) <- allResults, m == n]
      sortedExact = sortBy (\e1 e2 -> complexity e1 < complexity e2) exact
  in if null exact
     then findClosest n allResults
     else sortedExact