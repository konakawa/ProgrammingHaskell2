data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]
data Op = EVAL_ADD Expr | ADD Int | EVAL_MUL Expr | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mul x y) c = eval x (EVAL_MUL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL_ADD y : c) n = eval y (ADD n : c)
exec (EVAL_MUL y : c) n = eval y (MUL n : c)
exec (ADD m : c)  n = exec c (m + n)
exec (MUL m : c)  n = exec c (m * n)

value :: Expr -> Int
value e = eval e []