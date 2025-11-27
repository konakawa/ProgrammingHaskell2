data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
          deriving Show

exec :: Code -> Stack -> Stack
exec []           s           = s
exec (PUSH n : c) s           = exec c (n : s)
exec (ADD : c)    (m : n : s) = exec c (n+m : s)

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

{-

prop: comp' e c = comp e ++ c

e = Val n:
  comp' (Val n) c
  (comp' を適用)
= PUSH n : c
  (++ の性質)
= [PUSH n] ++ c
  (comp を逆適用)
= comp (Val n) ++ c

e = Add x y:
  comp' (Add x y) c
  (comp' を適用)
= comp' x (comp' y (ADD : c))
  (帰納法の仮定)
= comp' x (comp y ++ (ADD : c))
  (帰納法の仮定)
= comp x ++ (comp y ++ (ADD : c))
  (++ の性質)
= comp x ++ (comp y ++ ([ADD] ++ c))
  (++ の結合律)
= comp x ++ comp y ++ [ADD] ++ c
  (comp を逆適用)
= comp (Add x y) ++ c

-}